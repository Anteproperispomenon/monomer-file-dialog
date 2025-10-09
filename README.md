# Monomer File Dialogs

I made this because I couldn't get [tinyfiledialogs](https://github.com/mtolly/tinyfiledialogs) to link
properly on Windows anymore. Hopefully it'll work.

Note that it uses [`OsPath`](https://hackage.haskell.org/package/filepath-1.5.4.0/docs/System-OsPath.html)
instead of `FilePath`, to improve safety and performance(?).

## Usage

You'll need to add a `FileDialogModel` to your app's main model. 
When creating your model's initial value, you can use `defFileModel`/`defFileModelOpen`
or `defFileModelSave` as the `FileDialogModel`'s initial value.
If you want to change between open and save mode, you can use
`setOpen`/`setSave` together with [(%~)](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Operators.html#v:-37--126-) from [Control.Lens.Operators](https://hackage-content.haskell.org/package/lens-5.3.5/docs/Control-Lens-Operators.html).

## Notes

When navigating through the directories, it doesn't actually
change the current working directory. This is because the present
working directory is a global state, and it doesn't work well when
there are multiple working threads. Instead, it keeps an `OsPath`
value in the working model that changes as you navigate through
the file system. 

## Everything Else

For more information, check https://github.com/fjvallarino/monomer.
