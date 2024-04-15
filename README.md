<!-- BEGIN_ACTION_DOC -->
# Reverse Dependency Test of R Packages

### Description
GitHub Action for reverse dependency check of R packages
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
