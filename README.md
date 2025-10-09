# Monomer File Dialogs

I made this because I couldn't get [tinyfiledialogs](https://github.com/mtolly/tinyfiledialogs) to link
properly on Windows anymore. Hopefully it'll work.

Note that it uses [`OsPath`](https://hackage.haskell.org/package/filepath-1.5.4.0/docs/System-OsPath.html)
instead of `FilePath`, to improve safety and performance(?).

## Notice

`monomer-file-dialog` is still *very* early in development. It has not
been tested for memory usage/CPU usage/etc... 

## Setup

You'll probably want to follow the [monomer setup tutorial](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md)
and clone [`monomer-starter`](https://github.com/fjvallarino/monomer-starter) as a basis for
your project. However, you'll have to change a few things in `stack.yaml` to get it to work:

* After `resolver:`, change `lts-22.4` to `lts-23.28`. You might be able to use other revisions, but that's the version I'm using.
* In `extra-deps`, change `monomer-1.6.0.0` to `monomer-1.6.0.1`. This seems to fix some package compatibility issues.
* Also in `extra-deps`, add `monomer-hagrid-0.4.0.1` and `sdl2-2.5.5.1`. 
* Finally, add `git: https://github.com/Anteproperispomenon/monomer-file-dialog` followed by `commit: ...`, where the `...` is replaced by the latest commit of `monomer-file-dialog`.

In the end, your `extra-deps` should look like this:

```yaml
extra-deps:
- nanovg-0.8.1.0
- monomer-1.6.0.1
- trie-simple-0.4.4
- sdl2-2.5.5.1
- monomer-hagrid-0.4.0.1
- git: https://github.com/Anteproperispomenon/monomer-file-dialog
  commit: 4adf1ef5c9b8b0020773daa37da9a4ac3f8f63fd
```

After that, you can follow the other `monomer` tutorials to get
your GUI working.

## Usage

You'll need to add a `FileDialogModel` to your app's main model. 
When creating your model's initial value, you can use `defFileModel`/`defFileModelOpen`
or `defFileModelSave` as the `FileDialogModel`'s initial value.
If you want to change between open and save mode, you can use
`setOpen`/`setSave` together with [(%~)](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Operators.html#v:-37--126-) from [Control.Lens.Operators](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Operators.html).

e.g. `[Model (model & fileDialogModel %~ setSave)]`.



## Notes

When navigating through the directories, it doesn't actually
change the current working directory. This is because the present
working directory is a global state, and it doesn't work well when
there are multiple working threads. Instead, it keeps an `OsPath`
value in the working model that changes as you navigate through
the file system. 

## Everything Else

For more information, check https://github.com/fjvallarino/monomer.
