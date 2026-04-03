# dlange

> Compute the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a matrix.

<section class="usage">

## Usage

```javascript
var dlange = require( '@stdlib/lapack/base/dlange' );
```

#### dlange( order, norm, M, N, A, LDA, WORK, strideWORK )

Computes the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real `M`-by-`N` matrix `A`.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Column-major 2x2 matrix: [ [1, 3], [2, 4] ]
var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var WORK = new Float64Array( 2 );

var result = dlange( 'column-major', 'one-norm', 2, 2, A, 2, WORK, 1 );
// returns 7.0
```

The function has the following parameters:

-   **order**: storage layout (`'row-major'` or `'column-major'`).
-   **norm**: specifies the norm to compute. Must be one of `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **A**: input [`Float64Array`][mdn-float64array] containing the matrix.
-   **LDA**: leading dimension of `A`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array] of length at least `M` when `norm` is `'inf-norm'`; otherwise not referenced.
-   **strideWORK**: stride length for `WORK`.

#### dlange.ndarray( norm, M, N, A, strideA1, strideA2, offsetA, WORK, strideWORK, offsetWORK )

Computes the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real `M`-by-`N` matrix `A` using alternative indexing semantics.

```javascript
var Float64Array = require( '@stdlib/array/float64' );

// Column-major 2x2 matrix: [ [1, 3], [2, 4] ]
var A = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
var WORK = new Float64Array( 2 );

var result = dlange.ndarray( 'one-norm', 2, 2, A, 1, 2, 0, WORK, 1, 0 );
// returns 7.0
```

The function has the following parameters:

-   **norm**: specifies the norm to compute. Must be one of `'max'`, `'one-norm'`, `'inf-norm'`, or `'frobenius'`.
-   **M**: number of rows of `A`.
-   **N**: number of columns of `A`.
-   **A**: input [`Float64Array`][mdn-float64array] containing the matrix.
-   **strideA1**: stride of the first dimension of `A`.
-   **strideA2**: stride of the second dimension of `A`.
-   **offsetA**: starting index for `A`.
-   **WORK**: workspace [`Float64Array`][mdn-float64array].
-   **strideWORK**: stride length for `WORK`.
-   **offsetWORK**: starting index for `WORK`.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `dlange` returns the value of the specified norm. The available norms are:
    -   `'max'`: maximum absolute value of any element, `max(abs(A[i,j]))`.
    -   `'one-norm'`: maximum column sum of absolute values (1-norm).
    -   `'inf-norm'`: maximum row sum of absolute values (infinity norm).
    -   `'frobenius'`: square root of the sum of squares of all elements (Frobenius norm).
-   When `M` or `N` is zero, the function returns `0.0`.
-   The `WORK` array is only referenced when `norm` is `'inf-norm'` and must have length at least `M`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Float64Array = require( '@stdlib/array/float64' );
var dlange = require( '@stdlib/lapack/base/dlange' );

// 3x4 column-major matrix:
// A = [  1.0  -4.0   7.0  -2.0 ]
//     [ -3.0   5.0  -8.0   6.0 ]
//     [  2.0  -1.0   9.0  -3.0 ]
var A = new Float64Array( [
    1.0, -3.0, 2.0,
    -4.0, 5.0, -1.0,
    7.0, -8.0, 9.0,
    -2.0, 6.0, -3.0
] );
var WORK = new Float64Array( 3 );

// Max norm: largest absolute value
var maxNorm = dlange( 'column-major', 'max', 3, 4, A, 3, WORK, 1 );
console.log( 'Max norm:', maxNorm );
// => Max norm: 9.0

// One norm: maximum column sum
var oneNorm = dlange( 'column-major', 'one-norm', 3, 4, A, 3, WORK, 1 );
console.log( 'One norm:', oneNorm );
// => One norm: 24.0

// Infinity norm: maximum row sum
var infNorm = dlange( 'column-major', 'inf-norm', 3, 4, A, 3, WORK, 1 );
console.log( 'Infinity norm:', infNorm );
// => Infinity norm: 22.0

// Frobenius norm: sqrt of sum of squares
var frobNorm = dlange( 'column-major', 'frobenius', 3, 4, A, 3, WORK, 1 );
console.log( 'Frobenius norm:', frobNorm );
// => Frobenius norm: ~17.29
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
