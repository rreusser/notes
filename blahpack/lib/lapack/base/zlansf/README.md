# zlansf

> Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a complex symmetric matrix stored in Rectangular Full Packed (RFP) format.

<section class="usage">

## Usage

```javascript
var zlansf = require( '@stdlib/lapack/base/zlansf' );
```

#### zlansf( norm, transr, uplo, N, A, WORK )

Returns the norm of a complex symmetric matrix in RFP format.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var A = new Complex128Array( [ 1.0, 2.0, 5.0, -1.0, 2.0, 1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var result = zlansf( 'max', 'no-transpose', 'upper', 3, A, WORK );
// returns <number>
```

#### zlansf.ndarray( norm, transr, uplo, N, A, strideA, offsetA, WORK, strideWORK, offsetWORK )

Returns the norm with ndarray-style arguments.

```javascript
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );

var A = new Complex128Array( [ 1.0, 2.0, 5.0, -1.0, 2.0, 1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] );
var WORK = new Float64Array( 3 );

var result = zlansf.ndarray( 'max', 'no-transpose', 'upper', 3, A, 1, 0, WORK, 1, 0 );
// returns <number>
```

</section>

<section class="notes">

## Notes

- `norm` specifies the value to compute: `'max'` (max absolute value), `'one-norm'` (one-norm), `'inf-norm'` (infinity norm), or `'frobenius'` (Frobenius norm).
- `transr` specifies whether the RFP format is `'no-transpose'` or `'conjugate-transpose'`.
- `uplo` specifies whether the upper (`'upper'`) or lower (`'lower'`) triangle is stored.
- `WORK` must have length at least `N` for `'one-norm'` and `'inf-norm'` norms.

</section>
