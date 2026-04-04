/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len, no-lonely-if, no-unused-vars, no-mixed-operators, require-jsdoc, max-lines, stdlib/jsdoc-private-annotation */

'use strict';


// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansf = require( './../lib/base.js' );


// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Pack a complex symmetric matrix (stored as column-major full in interleaved.
* Float64 re/im pairs) into RFP format.
*
* @param {string} transr - 'no-transpose' or 'conjugate-transpose'
* @param {string} uplo - 'upper' or 'lower'
* @param {number} n - order of the matrix
* @param {Float64Array} full - n*n complex entries, column-major, interleaved re/im
* @returns {Complex128Array} RFP packed array
*/
function packRFP( transr, uplo, n, full ) {
	var rfpLen = Math.floor( n * ( n + 1 ) / 2 );
	var rfp = new Float64Array( rfpLen * 2 );
	var normalTransr = ( transr === 'no-transpose' );
	var upper = ( uplo === 'upper' );
	var k;
	var lda;
	var nRows;
	var nCols;
	var row;
	var col;
	var srcRow;
	var srcCol;
	var srcIdx;
	var dstIdx;

	// The RFP format stores the matrix in a rectangle.

	// For normal (non-transposed) format:

	//   n odd:  nRows = n, nCols = (n+1)/2, LDA = n

	//   n even: nRows = n+1, nCols = n/2, LDA = n+1

	// For transposed format:

	//   n odd:  nRows = (n+1)/2, nCols = n, LDA = (n+1)/2

	//   n even: nRows = n/2, nCols = n+1, LDA = n/2

	k = Math.floor( n / 2 );

	if ( n % 2 === 1 ) {
		// n is odd
		if ( normalTransr ) {
			lda = n;
			nRows = n;
			nCols = Math.floor( ( n + 1 ) / 2 );
		} else {
			lda = Math.floor( ( n + 1 ) / 2 );
			nRows = Math.floor( ( n + 1 ) / 2 );
			nCols = n;
		}
	} else {
		// n is even
		if ( normalTransr ) {
			lda = n + 1;
			nRows = n + 1;
			nCols = k;
		} else {
			lda = k;
			nRows = k;
			nCols = n + 1;
		}
	}

	// We need to map (row, col) in the RFP rectangle to (srcRow, srcCol) in the full matrix. // eslint-disable-line max-len
	// Then copy full[srcRow, srcCol] -> rfp[row + col*lda].

	// The mapping depends on n parity, transr, and uplo.
	// Rather than implement all 8 cases, use an indirect approach:
	// Build the "normal, not-transposed" RFP, then transpose if needed.

	// For normal transr, we fill rfp[row + col*lda] directly.
	// For transposed transr, we fill by reversing row/col roles.

	// Build the column-major RFP rectangle.
	for ( col = 0; col < nCols; col++ ) {
		for ( row = 0; row < nRows; row++ ) {
			if ( normalTransr ) {
				mapNormal( n, upper, row, col, rfp, full );
			} else {
				// transposed: the RFP rectangle is the transpose of the normal one,
				// So rfp_T[row, col] = rfp_N[col, row].
				// We map (col, row) through the normal mapping.
				mapNormal( n, upper, col, row, rfp, full );
			}
		}
	}

	return new Complex128Array( rfp.buffer );

	function mapNormal( n2, isUpper, r, c, dst, src ) {
		var halfN = Math.floor( ( n2 + 1 ) / 2 );
		var isOdd = ( n2 % 2 === 1 );
		var dIdx;
		var sIdx;
		var sR;
		var sC;

		if ( isOdd ) {
			// n is odd, LDA = n, rectangle is n x halfN
			if ( isUpper ) {
				// n odd, upper, normal:
				// Mapping (k = floor(n/2)):
				//   R <= k+c: A(r, c+k) from upper triangle
				//   R > k+c: A(c, r-k-1) from upper triangle (transposed)
				if ( r <= k + c ) {
					sR = r;
					sC = c + k;
				} else {
					sR = c;
					sC = r - k - 1;
				}
			} else {
				// n odd, lower, normal:
				// From Fortran docs for n=5, lower, normal:
				//   Col 0: 00 33 43
				//   Col 1: 10 11 44
				//   Col 2: 20 21 22
				//   Col 3: 30 31 32
				//   Col 4: 40 41 42
				// Wait, the rectangle is n x halfN = 5 x 3
				// Col 0: 00 33 43  -> rfp[0,0]=A(0,0) rfp[1,0]=A(3,3) rfp[2,0]=A(4,3)
				// Hmm no, from the Fortran doc:
				//         RFP A (lower, normal)
				//        00 33 43
				//        10 11 44
				//        20 21 22
				//        30 31 32
				//        40 41 42
				// So Col 0: 00 10 20 30 40 => A(0,0) A(1,0) A(2,0) A(3,0) A(4,0)
				// Col 1: 33 11 21 31 41 => A(3,3) A(1,1) A(2,1) A(3,1) A(4,1)
				// Col 2: 43 44 22 32 42 => A(4,3) A(4,4) A(2,2) A(3,2) A(4,2)

				// Pattern for n=5 odd, lower, normal (k=2, halfN=3):
				// rfp[r, c]:
				//   c=0: r=0..4 -> A(r, 0) (first column of lower AP)
				//   c=1: r=0 -> A(3,3), r=1..4 -> A(r, 1) but shifted...
				//   Actually: rfp[0,1] = A(3,3), rfp[1,1] = A(1,1) ?
				// Hmm, the table shows:
				//   Row0: 00 33 43
				//   Row1: 10 11 44
				//   Row2: 20 21 22
				//   Row3: 30 31 32
				//   Row4: 40 41 42
				// Reading column-major: col0 = [00,10,20,30,40], col1 = [33,11,21,31,41], col2 = [43,44,22,32,42] // eslint-disable-line max-len

				// For c=0: rfp[r,0] = A(r, 0) -- lower triangle entries
				// For c=1: rfp[0,1] = A(3,3), rfp[1,1] = A(1,1), rfp[2,1] = A(2,1), rfp[3,1] = A(3,1), rfp[4,1] = A(4,1) // eslint-disable-line max-len
				// For c=2: rfp[0,2] = A(4,3), rfp[1,2] = A(4,4), rfp[2,2] = A(2,2), rfp[3,2] = A(3,2), rfp[4,2] = A(4,2) // eslint-disable-line max-len

				// The pattern:
				// For c >= 1, first c rows come from upper triangle transpose of last c cols of AP lower // eslint-disable-line max-len
				//   rfp[r, c] for r < c: A(c+k, r+k+1) -- transposed entries
				// Wait, let me check: rfp[0,1] = A(3,3). c=1, r=0: A(1+2, 0+2+1) = A(3,3) YES // eslint-disable-line max-len
				//   rfp[0,2] = A(4,3). c=2, r=0: A(2+2, 0+2+1) = A(4,3) YES
				//   rfp[1,2] = A(4,4). c=2, r=1: A(2+2, 1+2+1) = A(4,4) YES
				// For r >= c:
				//   rfp[r, c] = A(r, c)  -- direct lower triangle
				//   rfp[1,1] = A(1,1) YES
				//   rfp[2,1] = A(2,1) YES
				//   rfp[4,0] = A(4,0) YES

				if ( r < c ) {
					sR = c + k;
					sC = r + k + 1;
				} else {
					sR = r;
					sC = c;
				}
			}
		} else {
			// n is even, LDA = n+1, rectangle is (n+1) x k where k=n/2
			var halfK = k; // n/2
			if ( isUpper ) {
				// From Fortran docs for n=6, upper, normal:
				//   RFP:
				//   03 04 05
				//   13 14 15
				//   23 24 25
				//   33 34 35
				//   00 44 45
				//   01 11 55
				//   02 12 22
				// Rectangle is 7 x 3 (n+1=7, k=3)
				// Col 0: 03 13 23 33 00 01 02
				// Col 1: 04 14 24 34 44 11 12
				// Col 2: 05 15 25 35 45 55 22

				// Pattern (n=6, k=3):
				// rfp[r, c] for r <= k+c (r <= 3+c):
				//   A(r, c+k) -- upper triangle
				//   rfp[0,0] = A(0,3)=03 YES, rfp[3,0] = A(3,3)=33 YES
				//   rfp[0,1] = A(0,4)=04 YES, rfp[4,1] = A(4,4)=44 YES
				// rfp[r, c] for r > k+c:
				//   A(c, r-k-1) -- transposed
				//   rfp[4,0]: r=4 > 3+0=3 -> A(0, 4-3-1) = A(0,0) = 00 YES
				//   rfp[5,0]: A(0, 5-4) = A(0,1) = 01 YES
				//   rfp[6,0]: A(0, 6-4) = A(0,2) = 02 YES
				//   rfp[5,1]: r=5 > 4 -> A(1, 5-4) = A(1,1) = 11 YES

				if ( r <= halfK + c ) {
					sR = r;
					sC = c + halfK;
				} else {
					sR = c;
					sC = r - halfK - 1;
				}
			} else {
				// n=6, lower, normal:
				//   RFP:
				//   33 43 53
				//   00 44 54
				//   10 11 55
				//   20 21 22
				//   30 31 32
				//   40 41 42
				//   50 51 52
				// Rectangle is 7 x 3
				// Col 0: 33 00 10 20 30 40 50
				// Col 1: 43 44 11 21 31 41 51
				// Col 2: 53 54 55 22 32 42 52

				// Pattern (n=6, k=3):
				// rfp[r, c] for r < c+1:
				//   Transposed from upper part of lower AP
				//   rfp[0,0] = A(3,3) -> r=0,c=0: r < 1 -> A(c+k, r+k) = A(3, 3) YES
				//   rfp[0,1] = A(4,3) -> r=0,c=1: r < 2 -> A(1+3, 0+3) = A(4,3) YES
				//   rfp[1,1] = A(4,4) -> r=1,c=1: r < 2 -> A(1+3, 1+3) = A(4,4) YES
				//   rfp[0,2] = A(5,3) -> r=0,c=2: r < 3 -> A(2+3, 0+3) = A(5,3) YES
				// rfp[r, c] for r >= c+1:
				//   rfp[1,0] = A(0,0) -> r=1,c=0: r >= 1 -> A(r-c-1, c) = A(0,0) YES
				//   rfp[2,0] = A(1,0) -> A(1,0) YES
				//   rfp[3,1] = A(2,1) -> r=3,c=1: A(3-1-1, 1) = A(1,1)? NO, should be A(2,1) // eslint-disable-line max-len
				//   Hmm, try A(r-1, c): rfp[1,0] = A(0,0) YES, rfp[3,1] = A(2,1) YES
				//   rfp[2,0] = A(1,0) YES, rfp[6,0] = A(5,0) YES
				//   rfp[2,1] = A(1,1) YES

				if ( r <= c ) {
					sR = c + halfK;
					sC = r + halfK;
				} else {
					sR = r - 1;
					sC = c;
				}
			}
		}

		sIdx = ( sC * n2 + sR ) * 2;
		dIdx = ( c * lda + r ) * 2;
		if ( normalTransr ) {
			dst[ dIdx ] = src[ sIdx ];
			dst[ dIdx + 1 ] = src[ sIdx + 1 ];
		} else {
			// For transposed: the destination is rfp_T[row, col] which means
			// We store at position (row + col*lda_T) where lda_T = halfN for odd, k for even // eslint-disable-line max-len
			var ldaT = lda;
			dIdx = ( r * ldaT + c ) * 2;

			// But wait, we're called with mapNormal(n, upper, col, row, ...) for transposed. // eslint-disable-line max-len

			// So (r, c) here is actually (col_of_rfp, row_of_rfp), and we map through normal. // eslint-disable-line max-len

			// The destination should be rfp[row_of_rfp + col_of_rfp * lda_T].

			// Since the caller swapped: the r here = col of original rfp, c here = row of original rfp. // eslint-disable-line max-len

			// rfp_T is stored column-major with LDA = lda_T.

			// rfp_T[orig_row, orig_col] = rfp_T[c_here, r_here] stored at c_here + r_here * lda_T // eslint-disable-line max-len
			dIdx = ( r * ldaT + c ) * 2;
			dst[ dIdx ] = src[ sIdx ];
			dst[ dIdx + 1 ] = src[ sIdx + 1 ];
		}
	}
}

/**
* Create a full complex symmetric matrix of order N (column-major, interleaved).
* Uses a simple deterministic pattern.
*/
function makeFullSymmetric( n ) {
	var full = new Float64Array( n * n * 2 );
	var i;
	var j;
	var re;
	var im;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			re = ( i + 1 ) * 0.5 + j * 0.3;
			im = ( j - i ) * 0.2 + 0.1;

			// A(i,j) = A(j,i) since symmetric
			full[ ( j * n + i ) * 2 ] = re;
			full[ ( j * n + i ) * 2 + 1 ] = im;
			full[ ( i * n + j ) * 2 ] = re;
			full[ ( i * n + j ) * 2 + 1 ] = im;
		}
	}
	return full;
}

/**
* Compute the expected max norm of a symmetric matrix.
*/
function expectedMax( n, full ) {
	var val = 0.0;
	var re;
	var im;
	var t;
	var i;
	var j;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			re = full[ ( j * n + i ) * 2 ];
			im = full[ ( j * n + i ) * 2 + 1 ];
			t = Math.sqrt( re * re + im * im );
			if ( val < t || t !== t ) {
				val = t;
			}
		}
	}
	return val;
}

/**
* Compute the expected one-norm (= inf-norm for symmetric) of a symmetric matrix.
*/
function expectedOneNorm( n, full ) {
	var colSums = new Float64Array( n );
	var val;
	var re;
	var im;
	var i;
	var j;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < n; i++ ) {
			re = full[ ( j * n + i ) * 2 ];
			im = full[ ( j * n + i ) * 2 + 1 ];
			colSums[ j ] += Math.sqrt( re * re + im * im );
		}
	}
	val = 0.0;
	for ( j = 0; j < n; j++ ) {
		if ( val < colSums[ j ] || colSums[ j ] !== colSums[ j ] ) {
			val = colSums[ j ];
		}
	}
	return val;
}

/**
* Compute the expected Frobenius norm of a symmetric matrix.
*/
function expectedFrobenius( n, full ) {
	var sum = 0.0;
	var re;
	var im;
	var i;
	var j;
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < n; i++ ) {
			re = full[ ( j * n + i ) * 2 ];
			im = full[ ( j * n + i ) * 2 + 1 ];
			sum += re * re + im * im;
		}
	}
	return Math.sqrt( sum );
}


// TESTS //

test( 'zlansf: N=0', function t() {
	var result;
	var WORK;
	var A;

	A = new Complex128Array( 0 );
	WORK = new Float64Array( 1 );
	result = zlansf( 'max', 'no-transpose', 'upper', 0, A, 1, 0, WORK, 1, 0 );
	assert.strictEqual( result, 0.0, 'N=0 returns 0' );
});

test( 'zlansf: N=1', function t() {
	var result;
	var WORK;
	var A;

	A = new Complex128Array( [ 3.0, 4.0 ] );
	WORK = new Float64Array( 1 );
	result = zlansf( 'max', 'no-transpose', 'upper', 1, A, 1, 0, WORK, 1, 0 );
	assertClose( result, 5.0, 1e-14, 'N=1 returns |A(0)|' );
});

// Test all 8 RFP code paths for max norm, one-norm, inf-norm, frobenius
var transrs = [ 'no-transpose', 'conjugate-transpose' ];
var uplos = [ 'upper', 'lower' ];
var sizes = [ 3, 4, 5, 6 ]; // mix of odd and even

sizes.forEach( function each( n ) {
	var full = makeFullSymmetric( n );
	var expMax = expectedMax( n, full );
	var expOne = expectedOneNorm( n, full );
	var expFro = expectedFrobenius( n, full );

	transrs.forEach( function each2( transr ) {
		uplos.forEach( function each3( uplo ) {
			var label = 'n=' + n + ', transr=' + transr + ', uplo=' + uplo;

			test( 'zlansf: max norm, ' + label, function t() {
				var result;
				var WORK;
				var rfp;

				rfp = packRFP( transr, uplo, n, full );
				WORK = new Float64Array( n );
				result = zlansf( 'max', transr, uplo, n, rfp, 1, 0, WORK, 1, 0 );
				assertClose( result, expMax, 1e-13, 'max norm' );
			});

			test( 'zlansf: one-norm, ' + label, function t() {
				var result;
				var WORK;
				var rfp;

				rfp = packRFP( transr, uplo, n, full );
				WORK = new Float64Array( n );
				result = zlansf( 'one-norm', transr, uplo, n, rfp, 1, 0, WORK, 1, 0 );
				assertClose( result, expOne, 1e-13, 'one-norm' );
			});

			test( 'zlansf: inf-norm, ' + label, function t() {
				var result;
				var WORK;
				var rfp;

				rfp = packRFP( transr, uplo, n, full );
				WORK = new Float64Array( n );
				result = zlansf( 'inf-norm', transr, uplo, n, rfp, 1, 0, WORK, 1, 0 );
				assertClose( result, expOne, 1e-13, 'inf-norm (= one-norm for symmetric)' );
			});

			test( 'zlansf: frobenius norm, ' + label, function t() {
				var result;
				var WORK;
				var rfp;

				rfp = packRFP( transr, uplo, n, full );
				WORK = new Float64Array( n );
				result = zlansf( 'frobenius', transr, uplo, n, rfp, 1, 0, WORK, 1, 0 );
				assertClose( result, expFro, 1e-12, 'frobenius norm' );
			});
		});
	});
});
