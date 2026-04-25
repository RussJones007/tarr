# Plan for a More Complete `clustered` S3 Class

This document outlines a plan to make the `clustered` S3 class (returned by `scan_cluster()`) more complete, robust, and self‑documenting.

---

## 1. Current State

### 1.1 Class structure

- `scan_cluster()` currently returns an object constructed like:

  - Based on an `sf` object with `POINT` geometry.
  - Columns include:
    - `cluster` (integer labels; `0` for noise).
    - `point_type` (currently `"core"` or `"noise"`).
  - Attributes:
    - `"scan_object"`: the underlying result from `dbscan::dbscan()`, `dbscan::hdbscan()`, or `dbscan::optics()`/`extractXi()`.
    - `"eps_unit"`: the distance unit used for `eps` (e.g., `"foot"`, `"meter"`).

- Class is set via:

  ```r
  class(ret) <- c("clustered", class(points))
  ```

### 1.2 Existing methods/helpers

- Methods:
  - `print.clustered()`
  - `autoplot.clustered()`
  - `plot.clustered()`
  - `select.clustered()`

- Attribute access helpers:
  - `scan_object()` → returns the `"scan_object"` attribute.
  - `scan_unit()` → returns the `"eps_unit"` attribute.

- Other related helpers:
  - `clustered_credits()` → builds a caption string from the scan object.
  - Mapping helpers (`mapCluster`, `get_cluster_polys`, `get_poly`, etc.).

### 1.3 Gaps and issues

- No formal constructor for the `clustered` class.
- No dedicated validator; invariants are implicit.
- Class and attributes are set ad hoc inside `scan_cluster()`.
- dplyr verbs (e.g., `select()`) can drop attributes; `select.clustered()` partially works around this but does not handle all edge cases (e.g., dropping required columns).
- `print.clustered()` currently prints the raw `scan_object`, which is not ideal as the primary user‑facing summary.

---

## 2. Target Class Contract

Define a clear contract for a valid `clustered` object:

- Class:
  - `c("clustered", "sf", "data.frame")` (or `c("clustered", class(x))` when wrapping an `sf` object).

- Geometry:
  - Must be `POINT` geometry only.

- Required columns:
  - `cluster` (integer or factor cluster labels; `0` = noise).
  - `point_type` (e.g., `"core"`, `"border"`, `"noise"` — or at least `"core"` / `"noise"` consistently).

- Required attributes:
  - `"scan_object"`:
    - The underlying clustering result (class such as `"dbscan_fast"`, `"hdbscan"`, `"optics"`).
  - `"eps_unit"`:
    - Character label describing the distance unit used for `eps`.

- Optional metadata attributes (future‑friendly):
  - `"scan_method"` (e.g., `"DBSCAN"`, `"OPTICS"`, `"HDBSCAN"`).
  - `"scan_crs"` (CRS used for clustering).
  - `"orig_crs"` (original CRS of input points).
  - `"scan_call"` (optional: the call to `scan_cluster()`).

The constructor and validator should enforce these invariants.

---

## 3. Core Infrastructure to Implement

### 3.1 Low‑level constructor: `new_clustered()`

Implement an internal constructor:

```r
new_clustered <- function(x, scan_object, eps_unit, ..., class = character())
```

Responsibilities:

- Assume `x` is an `sf` object with `POINT` geometry.
- Attach:
  - `attr(x, "scan_object") <- scan_object`
  - `attr(x, "eps_unit") <- eps_unit`
  - any additional attributes passed via `...` if desired.
- Set class:
  - `class(x) <- c("clustered", class)`
- Perform only minimal structural checks; leave full checking to `validate_clustered()`.

All `clustered` objects should be created via this constructor (directly or via a higher‑level helper).

### 3.2 Validator: `validate_clustered()`

Implement a validator:

```r
validate_clustered <- function(x)
```

Responsibilities:

- Check `inherits(x, "sf")`.
