# zhbtrd

> Reduces a complex Hermitian band matrix to real tridiagonal form by unitary similarity transformation.

<section class="usage">

## Usage

```javascript
var zhbtrd = require( '@stdlib/lapack/base/zhbtrd' );
```

#### zhbtrd( order, vect, uplo, N, kd, AB, LDAB, d, e, Q, LDQ, WORK )

Reduces a complex Hermitian band matrix A to real symmetric tridiagonal form T by a unitary similarity transformation: Q\*\*H \* A \* Q = T.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 Hermitian tridiagonal (KD=1), upper band storage (LDAB=2):
var AB = new Complex128Array( [
    0.0, 0.0, 4.0, 0.0,
    1.0, 1.0, 5.0, 0.0,
    2.0, -1.0, 6.0, 0.0,
    3.0, 1.0, 7.0, 0.0
] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Complex128Array( 1 );
var WORK = new Complex128Array( 4 );

zhbtrd( 'column-major', 'none', 'upper', 4, 1, AB, 2, d, e, Q, 1, WORK );
// d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **vect**: specifies whether to form the unitary matrix Q (`'none'`, `'initialize'`, or `'update'`).
-   **uplo**: specifies whether the upper or lower triangle is stored (`'upper'` or `'lower'`).
-   **N**: order of the matrix A.
-   **kd**: number of super- (or sub-) diagonals.
-   **AB**: `Complex128Array` band matrix in band storage.
-   **LDAB**: leading dimension of `AB` (>= kd+1).
-   **d**: `Float64Array` output for diagonal elements (length N).
-   **e**: `Float64Array` output for off-diagonal elements (length N-1).
-   **Q**: `Complex128Array` unitary matrix (N-by-N if vect is not `'none'`).
-   **LDQ**: leading dimension of `Q`.
-   **WORK**: `Complex128Array` workspace (length N).

#### zhbtrd.ndarray( vect, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, d, strideD, offsetD, e, strideE, offsetE, Q, strideQ1, strideQ2, offsetQ, WORK, strideWORK, offsetWORK )

Reduces a complex Hermitian band matrix to real tridiagonal form using alternative indexing semantics.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Complex128Array( [
    0.0, 0.0, 4.0, 0.0,
    1.0, 1.0, 5.0, 0.0,
    2.0, -1.0, 6.0, 0.0,
    3.0, 1.0, 7.0, 0.0
] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Complex128Array( 1 );
var WORK = new Complex128Array( 4 );

zhbtrd.ndarray( 'none', 'upper', 4, 1, AB, 1, 2, 0, d, 1, 0, e, 1, 0, Q, 1, 1, 0, WORK, 1, 0 );
// d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   The diagonal elements of the Hermitian matrix must be real. The routine forces the diagonal real at the start of the reduction.
-   The real cosines and complex sines of the plane rotations are temporarily stored in `d` and `WORK` during the reduction. The tridiagonal elements are extracted from `AB` at the end.
-   For `vect = 'initialize'`, Q is initialized to the identity matrix before accumulating rotations. For `vect = 'update'`, Q must contain an existing unitary matrix on entry.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zhbtrd = require( '@stdlib/lapack/base/zhbtrd' );

var AB = new Complex128Array( [
    0.0, 0.0, 4.0, 0.0,
    1.0, 1.0, 5.0, 0.0,
    2.0, -1.0, 6.0, 0.0,
    3.0, 1.0, 7.0, 0.0
] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Complex128Array( 16 );
var WORK = new Complex128Array( 4 );

zhbtrd( 'column-major', 'initialize', 'upper', 4, 1, AB, 2, d, e, Q, 4, WORK );

console.log( 'Diagonal:', d );
console.log( 'Off-diagonal:', e );
```

</section>

<!-- /.examples -->

<!-- Section for related `stdlib` packages. Do not manually edit this section, as it is automatically populated. -->

<section class="related">

</section>

<!-- /.related -->

<!-- Section for all links. Make sure to keep an empty line after the `section` element and another before the `/section` close. -->

<section class="links">

[mdn-float64array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array

</section>

<!-- /.links -->
