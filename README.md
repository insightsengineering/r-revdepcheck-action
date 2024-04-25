# Reverse Dependency Test of R Packages

### Description
Executes `revdepcheck::revdep_check()` to check against potential breaking changes of reverse dependent packages (i.e. packages that depends on your package).

#### Limited number of reverse depenencies
This action is using `.revdeprefs.yaml` configuration file to control which reverse dependencies to check. It should store an array of package references using [`pkgdepends` syntax](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html) including references to GitHub packages. Example:
```yaml
- foo
- bar/baz
- aaa/bbb@ccc
```
If the configuration file is missing (or it's empty) then all reverse dependencies are used.

Please see [`revdepcheck`](https://revdepcheck.r-lib.org/) package documentation for details.

<!-- BEGIN_ACTION_DOC -->

### Action Type
Composite

### Author
Insights Engineering

### Inputs
* `github-token`:

  _Description_: Token to clone dependencies from GitHub repositories.

  _Required_: `false`

  _Default_: `""`

* `repository-path`:

  _Description_: Directory where the checked package has been cloned.

  _Required_: `false`

  _Default_: `.`

* `number-of-workers`:

  _Description_: Number of revdepcheck workers

  _Required_: `false`

  _Default_: `2`

* `timeout`:

  _Description_: Max timeout (in seconds) of revdepcheck process.

  _Required_: `false`

  _Default_: `1200`

* `additional-env-vars`:

  _Description_: Additional environment variables.
Example usage:
  additional-env-vars: |
    ABC=123
    XYZ=456


  _Required_: `false`

  _Default_: `""`

### Outputs
None
<!-- END_ACTION_DOC -->
