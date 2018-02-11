#[macro_use]
extern crate emacs;
extern crate git2;
extern crate libc;

use emacs::ToLisp;
use emacs::FromLisp;
use emacs::HandleFunc;
use emacs::{Env, Result, Value};
use git2::Repository;

emacs_plugin_is_GPL_compatible!();
emacs_module_init!(init);

fn test(env: &mut Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
    let sym = env.intern("default-directory")?;

    let val = env.call("symbol-value", &[sym])?;

    let val = String::from_lisp(env, val)?;

    env.message(&format!("VARIABLE: {}", val))?;

    let val = env.clone_to_lisp(100)?;
    Ok(val)
}

fn repository_status(env: &mut Env, args: &[Value], _data: *mut libc::c_void) -> Result<Value> {
    let path: String = if args.len() == 1 {
        args[0].to_owned(env)?
    } else {
        String::from_lisp(
            env,
            env.call("symbol-value", &[env.intern("default-directory")?])?
        )?
    };

    let repo = match Repository::discover(&path) {
        Ok(repo) => repo,
        _ => return Ok(env.intern("nil")?),
    };

    let statuses = match repo.statuses(None) {
        Ok(statuses) => statuses,
        _ => return Ok(env.intern("nil")?),
    };

    if statuses.is_empty() {
        return Ok(env.intern("nil")?);
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
                let ht: Value = unsafe { std::mem::transmute_copy(&hashtable) };
                env.call("puthash", &[path.to_lisp(env)?, env.intern(state)?, ht])?;
            }
        };
    }

    Ok(hashtable)
}

pub fn init(env: &mut Env) -> Result<Value> {
    env.message("Loading sidebar-git module")?;

    emacs_subrs!{
        test -> _test;
        repository_status -> _repository_status;
    }

    env.register(
        "sidebar-git-status",
        _repository_status,
        0..1,
        "Read files status from rust.\n
         Take an optional parameter to indicate the repository's path. \n
         If nil, `default-directory' is used.",
        std::ptr::null_mut(),
    )?;

    env.register(
        "sidebar-git-test",
        _test,
        0..0,
        "Function test",
        std::ptr::null_mut(),
    )?;

    env.provide("sidebar-git")
}
