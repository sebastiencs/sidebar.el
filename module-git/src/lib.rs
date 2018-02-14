#[macro_use]
extern crate emacs;
extern crate git2;
extern crate libc;
#[macro_use]
extern crate lazy_static;

use emacs::ToLisp;
use emacs::FromLisp;
use emacs::HandleFunc;
use emacs::{Env, Result, Value};
use git2::Repository;

use std::thread;
use std::sync::mpsc::channel;
use std::sync::mpsc::TryRecvError;
use std::sync::Mutex;

lazy_static! {
    static ref MUTEX: Mutex<()> = Mutex::default();
}

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

fn test(env: &mut Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
    env.clone_to_lisp(10)
}

fn string_to_lisp(env: &Env, string: Option<String>) -> Result<Value> {
    match string {
        Some(string) => string.to_lisp(env),
        _ => env.intern("nil")
    }
}

fn get_repository(env: &Env, path: Option<&Value>) -> Result<Option<Repository>> {
    let path: String = match path {
        Some(path) => path.to_owned(env)?,
        _ => String::from_lisp(env, env.call("symbol-value", &[env.intern("default-directory")?])?)?
    };

    match Repository::discover(path) {
        Ok(repo) => Ok(Some(repo)),
        _ => Ok(None)
    }
}

fn get_branches(env: &mut Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
    let repo = match get_repository(env, args.first())? {
        Some(repo) => repo,
        _ => return env.intern("nil")
    };

    let head = match repo.head() {
        Ok(head) => head,
        _ => return env.intern("nil")
    };

    let head_name = head.shorthand().map(|name| {
        String::from(name)
    });

    let branch = git2::Branch::wrap(head);

    let upstream_name = branch.upstream().ok().map(|up| {
        up.into_reference().shorthand().map(|name| { String::from(name) })
    }).map_or(None, |name| {
        name
    });

    env.call("cons", &[
        string_to_lisp(env, head_name)?,
        string_to_lisp(env, upstream_name)?,
    ])
}

struct Status {
    path: String,
    status: &'static str
}

fn repository_status(env: &mut Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {

    let mutex = MUTEX.try_lock();

    // Don't spawn more than 1 thread
    if let Err(_) = mutex {
        return env.intern("nil");
    };

    let path: String = match args.first() {
        Some(path) => path.to_owned(env)?,
        _ => String::from_lisp(env, env.call("symbol-value", &[env.intern("default-directory")?])?)?
    };

    let (send, recv) = channel();

    let child = thread::spawn(move || {

        let repo = match Repository::discover(path) {
            Ok(repo) => repo,
            _ => return None
        };

        let mut options = git2::StatusOptions::new();

        options.include_ignored(true)
               .include_untracked(true)
               .recurse_untracked_dirs(false)
               .recurse_ignored_dirs(false);

        let statuses = match repo.statuses(Some(&mut options)) {
            Ok(statuses) => statuses,
            _ => return None
        };

        let mut vec = Vec::with_capacity(statuses.len());

        for entry in statuses.iter() {
            if let Some(path) = entry.path() {
                let state = match entry.status() {
                    s if s.contains(git2::STATUS_INDEX_MODIFIED | git2::STATUS_WT_MODIFIED) => {
                        "changed"
                    }
                    s if s.contains(git2::STATUS_WT_MODIFIED) => "not-updated",
                    s if s.contains(git2::STATUS_INDEX_MODIFIED) => "updated",
                    s if s.contains(git2::STATUS_IGNORED) => "ignored",
                    s if s.intersects(git2::STATUS_WT_NEW | git2::STATUS_INDEX_NEW) => "untracked",
                    _ => "",
                };
                if !state.is_empty() {
                    vec.push(Status{
                        path: String::from(path),
                        status: state
                    });
                }
            };
        }

        send.send(()).unwrap();

        Some(vec)
    });

    while let Err(TryRecvError::Empty) = recv.try_recv() {
        env.call("thread-yield", &[])?;
    };

    let statuses = match child.join() {
        Ok(Some(statuses)) => statuses,
        _ => return env.intern("nil")
    };

    if statuses.is_empty() {
        return env.intern("nil");
    }

    let hashtable = env.call(
        "make-hash-table",
        &[
            env.intern(":test")?,
            env.intern("equal")?,
            env.intern(":size")?,
            env.clone_to_lisp(statuses.len() as i64)?,
        ],
    )?;

    for Status { ref path, status } in statuses {
        let ht: Value = unsafe { std::mem::transmute_copy(&hashtable) };
        env.call("puthash", &[path.to_lisp(env)?, env.intern(status)?, ht])?;
    }

    env.message("THREAD DONE")?;

    Ok(hashtable)
}

pub fn init(env: &mut Env) -> Result<Value> {
    env.message("Loading sidebar-git module")?;

    emacs_subrs!{
        test -> _test;
        get_branches -> _get_branches;
        repository_status -> _repository_status;
    }

    env.register(
        "sidebar-git-status",
        _repository_status,
        0..1,
        "Read files status from rust in a thread.\n
         Take an optional parameter to indicate the repository's path. \n
         If nil, `default-directory' is used.",
        std::ptr::null_mut(),
    )?;

    env.register(
        "sidebar-git-branches",
        _get_branches,
        0..1,
        "Retrieve head and its upstream branch from rust.\n
         Take an optional parameter to indicate the repository's path. \n
         If nil, `default-directory' is used.",
        std::ptr::null_mut(),
    )?;

    env.register(
        "sidebar-git-test",
        _test,
        0..1,
        "Function test",
        std::ptr::null_mut(),
    )?;

    env.provide("sidebar-git")
}
