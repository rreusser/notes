# zlags2

> Computes 2-by-2 unitary matrices U, V, and Q for the generalized upper (lower) triangular form.

<section class="usage">

## Usage

```javascript
var zlags2 = require( '@stdlib/lapack/base/zlags2' );
```

#### zlags2( upper, a1, a2, a3, b1, b2, b3 )

Computes 2-by-2 unitary matrices U, V, and Q such that if `upper` is `true`:

```text
  U**H * A * Q = ( x  0 )    V**H * B * Q = ( x  0 )
                 ( x  x )                    ( x  x )
```

or if `upper` is `false`:

```text
  U**H * A * Q = ( x  x )    V**H * B * Q = ( x  x )
                 ( 0  x )                    ( 0  x )
```

```javascript
var Complex128 = require( '@stdlib/complex/float64/ctor' );

var out = zlags2( true, 4.0, new Complex128( 2.0, 1.0 ), 3.0, 1.0, new Complex128( 0.5, 0.25 ), 2.0 );
// returns { csu: ~1.0, snuR: ~0.0, snuI: ~0.0, csv: ~1.0, snvR: ~0.0, snvI: ~0.0, csq: ~0.873, snqR: ~-0.436, snqI: ~-0.218 }
```

The function has the following parameters:

-   **upper**: `boolean` indicating whether the input matrices are upper triangular.
-   **a1**: real (1,1) element of matrix A.
-   **a2**: complex off-diagonal element of matrix A (`Complex128`).
-   **a3**: real (2,2) element of matrix A.
-   **b1**: real (1,1) element of matrix B.
-   **b2**: complex off-diagonal element of matrix B (`Complex128`).
-   **b3**: real (2,2) element of matrix B.

The function returns an object with properties `csu`, `snuR`, `snuI`, `csv`, `snvR`, `snvI`, `csq`, `snqR`, `snqI` defining the unitary matrices U, V, and Q.

</section>

<!-- /.usage -->

<section class="notes">

## Notes

-   `csu` and `csv` and `csq` are real cosines; `snuR`+`snuI`, `snvR`+`snvI`, `snqR`+`snqI` are real and imaginary parts of complex sines.
-   The unitary matrices have the form `U = ( csu, snu; -conj(snu), csu )`.

</section>

<!-- /.notes -->

<section class="examples">

## Examples

```javascript
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var zlags2 = require( '@stdlib/lapack/base/zlags2' );

var out = zlags2( true, 4.0, new Complex128( 2.0, 1.0 ), 3.0, 1.0, new Complex128( 0.5, 0.25 ), 2.0 );
console.log( out );
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
