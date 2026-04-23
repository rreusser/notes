<!--

@license Apache-2.0

Copyright (c) 2025 The Stdlib Authors.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-->

# dlagts

> Solves the system (T - lambda_I)_x = y or (T - lambda_I)__T_x = y using.

<section class="usage">

## Usage

```javascript
var dlagts = require( '@stdlib/lapack/base/dlagts' );
```

#### dlagts( job, N, a, strideA, b, strideB, c, strideC, d, strideD, IN, strideIN, y, strideY, tol )

Solves the system (T - lambda_I)_x = y or (T - lambda_I)__T_x = y using.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following parameters:

-   **job**: `job`.
-   **N**: number of columns.
-   **a**: `a`.
-   **strideA**: stride length for `A`.
-   **b**: `b`.
-   **strideB**: stride length for `B`.
-   **c**: `c`.
-   **strideC**: stride length for `C`.
-   **d**: `d`.
-   **strideD**: stride length for `D`.
-   **IN**: input array `IN`.
-   **strideIN**: stride length for `IN`.
-   **y**: `y`.
-   **strideY**: stride length for `Y`.
-   **tol**: `tol`.

#### dlagts.ndarray( job, N, a, strideA, offsetA, b, strideB, offsetB, c, strideC, offsetC, d, strideD, offsetD, IN, strideIN, offsetIN, y, strideY, offsetY, tol )

Solves the system (T - lambda_I)_x = y or (T - lambda_I)__T_x = y using, using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// TODO: Add usage example
```

The function has the following additional parameters:

-   **offsetA**: starting index for `A`.
-   **offsetB**: starting index for `B`.
-   **offsetC**: starting index for `C`.
-   **offsetD**: starting index for `D`.
-   **offsetIN**: starting index for `IN`.
-   **offsetY**: starting index for `Y`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlagts()` corresponds to the [LAPACK][lapack] level routine [`dlagts`][lapack-dlagts].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var dlagts = require( '@stdlib/lapack/base/dlagts' );

// TODO: Add examples
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[lapack]: https://www.netlib.org/lapack/explore-html/

[lapack-dlagts]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__dlagts.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->