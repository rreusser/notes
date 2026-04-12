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

# zlarzt

> Forms the triangular factor T of a complex block reflector H = I - V\*T\*V^H.

<section class="usage">

## Usage

```javascript
var zlarzt = require( '@stdlib/lapack/base/zlarzt' );
```

#### zlarzt( order, direct, storev, N, K, V, LDV, TAU, strideTAU, T, LDT )

Forms the triangular factor T of a complex block reflector.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var K = 1;
var N = 3;
var V = new Complex128Array( [ 0.3, 0.1, -0.5, 0.2, 0.0, 0.0 ] );
var TAU = new Complex128Array( [ 0.8, -0.3 ] );
var T = new Complex128Array( K * K );

zlarzt( 'column-major', 'backward', 'rowwise', N, K, V, K, TAU, 1, T, K );
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **direct**: direction of reflector application (`'backward'`).
-   **storev**: storage of reflector vectors (`'rowwise'`).
-   **N**: order of the block reflector.
-   **K**: number of elementary reflectors.
-   **V**: [`Complex128Array`][@stdlib/array/complex128] matrix of reflector vectors.
-   **LDV**: leading dimension of `V`.
-   **TAU**: [`Complex128Array`][@stdlib/array/complex128] of scalar factors.
-   **strideTAU**: stride length for `TAU`.
-   **T**: output [`Complex128Array`][@stdlib/array/complex128] triangular matrix.
-   **LDT**: leading dimension of `T`.

#### zlarzt.ndarray( direct, storev, N, K, V, strideV1, strideV2, offsetV, TAU, strideTAU, offsetTAU, T, strideT1, strideT2, offsetT )

Forms the triangular factor T of a complex block reflector, using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );

var K = 1;
var N = 3;
var V = new Complex128Array( [ 0.3, 0.1, -0.5, 0.2, 0.0, 0.0 ] );
var TAU = new Complex128Array( [ 0.8, -0.3 ] );
var T = new Complex128Array( K * K );

zlarzt.ndarray( 'backward', 'rowwise', N, K, V, 1, K, 0, TAU, 1, 0, T, 1, K, 0 );
```

The function has the following additional parameters:

-   **direct**: direction of reflector application (`'backward'`).
-   **storev**: storage of reflector vectors (`'rowwise'`).
-   **N**: order of the block reflector.
-   **K**: number of elementary reflectors.
-   **V**: [`Complex128Array`][@stdlib/array/complex128] matrix of reflector vectors.
-   **strideV1**: stride of dimension 1 of `V`.
-   **strideV2**: stride of dimension 2 of `V`.
-   **offsetV**: starting index for `V`.
-   **TAU**: [`Complex128Array`][@stdlib/array/complex128] of scalar factors.
-   **strideTAU**: stride length for `TAU`.
-   **offsetTAU**: starting index for `TAU`.
-   **T**: output [`Complex128Array`][@stdlib/array/complex128] triangular matrix.
-   **strideT1**: stride of dimension 1 of `T`.
-   **strideT2**: stride of dimension 2 of `T`.
-   **offsetT**: starting index for `T`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Only `direct = 'backward'` and `storev = 'rowwise'` are currently supported, matching the reference LAPACK implementation.
-   All array strides and offsets are in complex elements (not Float64 units).

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarzt = require( '@stdlib/lapack/base/zlarzt' );

var K = 2;
var N = 5;

var V = new Complex128Array( [
    0.3, 0.1, -0.5, 0.2, 0.7, -0.4, 0.0, 0.0, 0.0, 0.0,
    0.1, 0.6, 0.4, -0.3, -0.2, 0.5, 0.0, 0.0, 0.0, 0.0
] );
var TAU = new Complex128Array( [ 0.8, -0.3, 0.5, 0.2 ] );
var T = new Complex128Array( K * K );

zlarzt.ndarray( 'backward', 'rowwise', N, K, V, 1, K, 0, TAU, 1, 0, T, 1, K, 0 );

console.log( reinterpret( T, 0 ) );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[@stdlib/array/complex128]: https://github.com/stdlib-js/array-complex128

</section>

<!-- /.links -->
