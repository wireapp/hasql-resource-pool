# 1.10.1.0

* Migrate to `Hasql-1.10`
* Require explicit `idle_in_transaction_session_timeout` and `statement_timeout` setting

# 1.9.1.3

* Support for `resource-pool-0.5.0.0`

# 1.9.1.2

* Breaking change: the API is made compatible with `hasql-1.9.1.2`
* Follow the type errors to correct missing connection settings, they should be self-explanatory.

# 0.6.0.0

* Upgrade to support [`reasource-pool`](https://github.com/scrive/pool) from the new maintainer.
* `acquireWith` no longer accepts the stripes number, due to semantic changes in `resource-pool`.


# 0.5.4.1

* Upgrade to support breaking changes of `hasql-1.6.3`. No breaking changes introduced to the library.

# 0.5.3.1

* `acquireWithStripes` renamed to `aquireWith` and allows specifying a custom `ConnectionGetter`.


# 0.5.3

* Initial release as a new package. The implementation is based on `hasql-pool` v0.5.2.2
and continues using `resource-pool` v0.2.x
(actually, a [fork with important performance and stats changes applied](https://github.com/bos/pool/pull/43)).
* The long-term plan is to make a switch to a [newer v0.3.x maintained by scrive](https://github.com/bos/pool/pull/43).
