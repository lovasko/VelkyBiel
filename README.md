# tw
Trailing whitespace detector `tw` command-line utility scans a set of files and
tests each line of each file for whitespace characters at its end.  The utility
processes only one line at a time and therefore can be used with files that
contain arbitrarily many lines.

## Usage
The utility accepts zero or more arguments: when the argument count is zero,
`stdin` is used, otherwise the arguments are used as paths to the files.

### Synopsis
```
tw [FILE ...]
```

## Return code
The `tw` utility returns `1` if any lines with trailing whitespace in any of
the files were found, `0` otherwise.

## Example
Following example examines all standard library C header files that were
shipped with the FreeBSD operating system version 10.0. Apparently, `stdlib.h`
contains one line with trailing whitespace, which is further proved by a small
`sed` script that quotes the incriminated line:

```sh
$ freebsd-version
10.0-RELEASE

$ tw /usr/include/std*.h
/usr/include/stdlib.h: 258

$ echo $?
1

$ sed -n '258s/\(.*\)/"\1"/p' /usr/include/stdlib.h
"__uint32_t "
```

## Build & install
To compile the utility, use the simple `make` command:
```sh
$ make
$ ls -l ./tw
-rwxr-xr-x  1 root  wheel  1877008 Jun 19 02:52 ./tw 
```

## License
2-clause BSD license. For more information please consult the
[LICENSE](LICENSE.md) file. In the case that you need a different license, feel
free to contact me.

## Author
Daniel Lovasko (daniel.lovasko@gmail.com)

