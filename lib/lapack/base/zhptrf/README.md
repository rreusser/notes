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

# zhptrf

> @license Apache-2.0.

<section class="usage">

## Usage

```javascript
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );
```

#### zhptrf( uplo, N, AP, IPIV )

@license Apache-2.0.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );

var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

zhptrf( 'lower', 3, AP, IPIV );
```

The function has the following parameters:

-   **uplo**: specifies whether the upper or lower triangular part is referenced.
-   **N**: number of columns.
-   **AP**: input array `AP`.
-   **IPIV**: input array `IPIV`.

#### zhptrf.ndarray( uplo, N, AP, strideAP, offsetAP, IPIV, strideIPIV, offsetIPIV )

@license Apache-2.0, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );

var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

zhptrf( 'lower', 3, AP, IPIV );
```

The function has the following additional parameters:

-   **strideAP**: stride length for `AP`.
-   **offsetAP**: starting index for `AP`.
-   **strideIPIV**: stride length for `IPIV`.
-   **offsetIPIV**: starting index for `IPIV`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `zhptrf()` corresponds to the [LAPACK][lapack] level routine [`zhptrf`][lapack-zhptrf].

</section>

<!-- /.notes -->

<section class="examples">

## Examples

<!-- eslint no-undef: "error" -->

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zhptrf = require( '@stdlib/lapack/base/zhptrf' );

var AP = new Complex128Array( [ 4, 0, 1, -2, 3, 1, 5, 0, 2, -1, 7, 0 ] );
var IPIV = new Int32Array( 3 );

zhptrf( 'lower', 3, AP, IPIV );
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

[lapack-zhptrf]: https://www.netlib.org/lapack/explore-html/d5/d2f/group__zhptrf.html

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->