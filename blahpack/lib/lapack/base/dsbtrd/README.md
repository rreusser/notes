# dsbtrd

> Reduces a real symmetric band matrix to tridiagonal form by orthogonal similarity transformation.

<section class="usage">

## Usage

```javascript
var dsbtrd = require( '@stdlib/lapack/base/dsbtrd' );
```

#### dsbtrd( order, vect, uplo, N, kd, AB, LDAB, d, e, Q, LDQ, WORK )

Reduces a real symmetric band matrix A to symmetric tridiagonal form T by an orthogonal similarity transformation: Q' \* A \* Q = T.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// 4x4 symmetric tridiagonal (KD=1), upper band storage (LDAB=2):
var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Float64Array( 1 );
var WORK = new Float64Array( 4 );

dsbtrd( 'column-major', 'none', 'upper', 4, 1, AB, 2, d, e, Q, 1, WORK );
// d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
// e => <Float64Array>[ 1.0, 2.0, 3.0 ]
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **vect**: `'none'` (do not form Q), `'initialize'` (form Q from identity), or `'update'` (update existing Q).
-   **uplo**: `'upper'` (upper triangle stored) or `'lower'` (lower triangle stored).
-   **N**: order of the matrix A (N >= 0).
-   **kd**: number of superdiagonals (if UPLO='upper') or subdiagonals (if UPLO='lower') of A.
-   **AB**: band matrix in band storage, dimension (LDAB, N).
-   **LDAB**: leading dimension of AB (LDAB >= KD+1).
-   **d**: output array for diagonal elements of T (length N).
-   **e**: output array for off-diagonal elements of T (length N-1).
-   **Q**: orthogonal transformation matrix (dimension LDQ x N); only referenced if vect is not `'none'`.
-   **LDQ**: leading dimension of Q.
-   **WORK**: workspace array (length N).

#### dsbtrd.ndarray( vect, uplo, N, kd, AB, strideAB1, strideAB2, offsetAB, d, strideD, offsetD, e, strideE, offsetE, Q, strideQ1, strideQ2, offsetQ, WORK, strideWORK, offsetWORK )

Same operation using ndarray-style indexing with explicit strides and offsets.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

var AB = new Float64Array( [ 0.0, 4.0, 1.0, 5.0, 2.0, 6.0, 3.0, 7.0 ] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Float64Array( 1 );
var WORK = new Float64Array( 4 );

dsbtrd.ndarray( 'none', 'upper', 4, 1, AB, 1, 2, 0, d, 1, 0, e, 1, 0, Q, 1, 1, 0, WORK, 1, 0 );
// d => <Float64Array>[ 4.0, 5.0, 6.0, 7.0 ]
// e => <Float64Array>[ 1.0, 2.0, 3.0 ]
```

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   Band storage format: for UPLO='upper', `AB(kd+1+i-j, j) = A(i,j)` for `max(1,j-kd) <= i <= j`; for UPLO='lower', `AB(1+i-j, j) = A(i,j)` for `j <= i <= min(n,j+kd)`.
-   During the reduction, the arrays `d` and `WORK` are temporarily used to store cosine and sine values of Givens rotations. The actual tridiagonal elements are extracted from AB after the reduction completes.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dsbtrd = require( '@stdlib/lapack/base/dsbtrd' );

// 4x4 symmetric band matrix with KD=2, lower storage:
//   3  1  2  0
//   1  4  1  2
//   2  1  5  1
//   0  2  1  6
var AB = new Float64Array( [
    3.0, 1.0, 2.0,  // col 1: diag, sub1, sub2
    4.0, 1.0, 2.0,  // col 2
    5.0, 1.0, 0.0,  // col 3
    6.0, 0.0, 0.0   // col 4
] );
var d = new Float64Array( 4 );
var e = new Float64Array( 3 );
var Q = new Float64Array( 16 );
var WORK = new Float64Array( 4 );

dsbtrd( 'column-major', 'initialize', 'lower', 4, 2, AB, 3, d, e, Q, 4, WORK );

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
[mdn-float32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array
[mdn-int32array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int32Array
[mdn-typed-array]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray

</section>

<!-- /.links -->
