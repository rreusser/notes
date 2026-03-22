

'use strict';

// MODULES //

var zgemv = require( '../../../../blas/base/zgemv/lib/base.js' );
var zlacgv = require( '../../zlacgv/lib/base.js' );
var zlarfg = require( '../../zlarfg/lib/base.js' );
var zscal = require( '../../../../blas/base/zscal/lib/base.js' );

// VARIABLES //

var ONE = new Float64Array( [ 1.0, 0.0 ] );
var ZERO = new Float64Array( [ 0.0, 0.0 ] );
var NEGONE = new Float64Array( [ -1.0, 0.0 ] );

// MAIN //

/**
* Reduces the first NB rows and columns of a complex general M-by-N matrix A
* to upper or lower real bidiagonal form by a unitary transformation
* Q^H * A * P, and returns the matrices X and Y which are needed to apply
* the transformation to the unreduced part of A.
*
* If M >= N, A is reduced to upper bidiagonal form; if M < N, to lower
* bidiagonal form.
*
* This is an auxiliary routine called by ZGEBRD.
*
* @private
* @param {NonNegativeInteger} M - number of rows
* @param {NonNegativeInteger} N - number of columns
* @param {integer} nb - number of leading rows and columns to reduce
* @param {Float64Array} A - input matrix (interleaved complex, M-by-N)
* @param {integer} strideA1 - stride of the first dimension of `A`
* @param {integer} strideA2 - stride of the second dimension of `A`
* @param {NonNegativeInteger} offsetA - starting index for `A`
* @param {Float64Array} d - real diagonal elements (length nb)
* @param {integer} strideD - stride length for `d`
* @param {NonNegativeInteger} offsetD - starting index for `d`
* @param {Float64Array} e - real off-diagonal elements (length nb)
* @param {integer} strideE - stride length for `e`
* @param {NonNegativeInteger} offsetE - starting index for `e`
* @param {Float64Array} TAUQ - complex scalars for Q reflectors (length nb)
* @param {integer} strideTAUQ - stride length for `TAUQ`
* @param {NonNegativeInteger} offsetTAUQ - starting index for `TAUQ`
* @param {Float64Array} TAUP - complex scalars for P reflectors (length nb)
* @param {integer} strideTAUP - stride length for `TAUP`
* @param {NonNegativeInteger} offsetTAUP - starting index for `TAUP`
* @param {Float64Array} X - output matrix (interleaved complex, M-by-NB)
* @param {integer} strideX1 - stride of the first dimension of `X`
* @param {integer} strideX2 - stride of the second dimension of `X`
* @param {NonNegativeInteger} offsetX - starting index for `X`
* @param {Float64Array} Y - output matrix (interleaved complex, N-by-NB)
* @param {integer} strideY1 - stride of the first dimension of `Y`
* @param {integer} strideY2 - stride of the second dimension of `Y`
* @param {NonNegativeInteger} offsetY - starting index for `Y`
*/
function zlabrd( M, N, nb, A, strideA1, strideA2, offsetA, d, strideD, offsetD, e, strideE, offsetE, TAUQ, strideTAUQ, offsetTAUQ, TAUP, strideTAUP, offsetTAUP, X, strideX1, strideX2, offsetX, Y, strideY1, strideY2, offsetY ) { // eslint-disable-line max-len, max-params
	/* @complex-arrays A, TAUQ, TAUP, X, Y */
	var alphaRe;
	var alphaIm;
	var alpha;
	var sa1;
	var sa2;
	var sx1;
	var sx2;
	var sy1;
	var sy2;
	var stq;
	var stp;
	var ia;
	var i;

	alpha = new Float64Array( 2 );

	if ( M <= 0 || N <= 0 ) {
		return;
	}

	// Convert complex-element strides to Float64 strides for matrices
	sa1 = strideA1 * 2;
	sa2 = strideA2 * 2;
	sx1 = strideX1 * 2;
	sx2 = strideX2 * 2;
	sy1 = strideY1 * 2;
	sy2 = strideY2 * 2;
	stq = strideTAUQ * 2;
	stp = strideTAUP * 2;

	if ( M >= N ) {
		// Reduce to upper bidiagonal form
		for ( i = 0; i < nb; i++ ) {
			// Update A(i:M-1, i)
			//
			// Fortran: CALL ZLACGV( I-1, Y(I,1), LDY )
			// Y(I,1) => row i, col 0 of Y => offsetY + i*sy1
			// stride = LDY => strideY2
			zlacgv( i, Y, strideY2, offsetY + i * sy1 );

			// Fortran: CALL ZGEMV('N', M-I+1, I-1, -ONE, A(I,1), LDA, Y(I,1), LDY, ONE, A(I,I), 1)
			// A(I,1) => row i, col 0 => offsetA + i*sa1
			// Y(I,1) => row i, col 0 => offsetY + i*sy1, stride=LDY => strideY2
			// A(I,I) => row i, col i => offsetA + i*sa1 + i*sa2, stride=1 => strideA1
			zgemv( 'N', M - i, i, NEGONE,
				A, strideA1, strideA2, offsetA + i * sa1,
				Y, strideY2, offsetY + i * sy1,
				ONE, A, strideA1, offsetA + i * sa1 + i * sa2
			);

			// Unconjugate Y(i, 0:i-1)
			zlacgv( i, Y, strideY2, offsetY + i * sy1 );

			// Fortran: CALL ZGEMV('N', M-I+1, I-1, -ONE, X(I,1), LDX, A(1,I), 1, ONE, A(I,I), 1)
			// X(I,1) => row i, col 0 => offsetX + i*sx1
			// A(1,I) => row 0, col i => offsetA + i*sa2, stride=1 => strideA1
			// A(I,I) => row i, col i => offsetA + i*sa1 + i*sa2, stride=1 => strideA1
			zgemv( 'N', M - i, i, NEGONE,
				X, strideX1, strideX2, offsetX + i * sx1,
				A, strideA1, offsetA + i * sa2,
				ONE, A, strideA1, offsetA + i * sa1 + i * sa2
			);

			// Generate elementary reflector H(i) to annihilate A(i+1:M-1, i)
			//
			// Fortran: ALPHA = A(I,I)
			// CALL ZLARFG(M-I+1, ALPHA, A(MIN(I+1,M),I), 1, TAUQ(I))
			// D(I) = DBLE(ALPHA)
			//
			// Save A(I,I) before zlarfg (Fortran uses local ALPHA variable)
			ia = offsetA + i * sa1 + i * sa2;
			alphaRe = A[ ia ];
			alphaIm = A[ ia + 1 ];
			alpha[ 0 ] = alphaRe;
			alpha[ 1 ] = alphaIm;
			zlarfg( M - i, alpha, 0,
				A, strideA1, offsetA + Math.min( i + 1, M - 1 ) * sa1 + i * sa2,
				TAUQ, offsetTAUQ + i * stq
			);
			d[ offsetD + i * strideD ] = alpha[ 0 ];

			if ( i < N - 1 ) {
				// Set A(i,i) = 1 for use as the reflector vector
				A[ ia ] = 1.0;
				A[ ia + 1 ] = 0.0;

				// Compute Y(i+1:N-1, i)
				//
				// Fortran: CALL ZGEMV('C', M-I+1, N-I, ONE, A(I,I+1), LDA, A(I,I), 1, ZERO, Y(I+1,I), 1)
				// A(I,I+1) => row i, col i+1 => offsetA + i*sa1 + (i+1)*sa2
				// A(I,I) => row i, col i => offsetA + i*sa1 + i*sa2, stride=1 => strideA1
				// Y(I+1,I) => row i+1, col i => offsetY + (i+1)*sy1 + i*sy2, stride=1 => strideY1
				zgemv( 'C', M - i, N - i - 1, ONE,
					A, strideA1, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2,
					A, strideA1, offsetA + i * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZGEMV('C', M-I+1, I-1, ONE, A(I,1), LDA, A(I,I), 1, ZERO, Y(1,I), 1)
				// A(I,1) => row i, col 0 => offsetA + i*sa1
				// A(I,I) => row i, col i => offsetA + i*sa1 + i*sa2, stride=1 => strideA1
				// Y(1,I) => row 0, col i => offsetY + i*sy2, stride=1 => strideY1
				zgemv( 'C', M - i, i, ONE,
					A, strideA1, strideA2, offsetA + i * sa1,
					A, strideA1, offsetA + i * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + i * sy2
				);

				// Fortran: CALL ZGEMV('N', N-I, I-1, -ONE, Y(I+1,1), LDY, Y(1,I), 1, ONE, Y(I+1,I), 1)
				// Y(I+1,1) => row i+1, col 0 => offsetY + (i+1)*sy1
				// Y(1,I) => row 0, col i => offsetY + i*sy2, stride=1 => strideY1
				// Y(I+1,I) => row i+1, col i => offsetY + (i+1)*sy1 + i*sy2, stride=1 => strideY1
				zgemv( 'N', N - i - 1, i, NEGONE,
					Y, strideY1, strideY2, offsetY + ( i + 1 ) * sy1,
					Y, strideY1, offsetY + i * sy2,
					ONE, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZGEMV('C', M-I+1, I-1, ONE, X(I,1), LDX, A(I,I), 1, ZERO, Y(1,I), 1)
				zgemv( 'C', M - i, i, ONE,
					X, strideX1, strideX2, offsetX + i * sx1,
					A, strideA1, offsetA + i * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + i * sy2
				);

				// Fortran: CALL ZGEMV('C', I-1, N-I, -ONE, A(1,I+1), LDA, Y(1,I), 1, ONE, Y(I+1,I), 1)
				// A(1,I+1) => row 0, col i+1 => offsetA + (i+1)*sa2
				zgemv( 'C', i, N - i - 1, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa2,
					Y, strideY1, offsetY + i * sy2,
					ONE, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZSCAL(N-I, TAUQ(I), Y(I+1,I), 1)
				// TAUQ(I) is a complex scalar at offsetTAUQ + i*stq
				zscal( N - i - 1, TAUQ.subarray( offsetTAUQ + i * stq, offsetTAUQ + i * stq + 2 ),
					Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Update A(i, i+1:N-1)
				//
				// Fortran: CALL ZLACGV(N-I, A(I,I+1), LDA)
				// A(I,I+1) => row i, col i+1, stride=LDA => strideA2
				zlacgv( N - i - 1, A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2 );

				// Fortran: CALL ZLACGV(I, A(I,1), LDA)
				zlacgv( i + 1, A, strideA2, offsetA + i * sa1 );

				// Fortran: CALL ZGEMV('N', N-I, I, -ONE, Y(I+1,1), LDY, A(I,1), LDA, ONE, A(I,I+1), LDA)
				// Note: A(I,1) stride=LDA => strideA2, A(I,I+1) stride=LDA => strideA2
				zgemv( 'N', N - i - 1, i + 1, NEGONE,
					Y, strideY1, strideY2, offsetY + ( i + 1 ) * sy1,
					A, strideA2, offsetA + i * sa1,
					ONE, A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2
				);

				// Unconjugate A(I, 1:I)
				zlacgv( i + 1, A, strideA2, offsetA + i * sa1 );

				// Fortran: CALL ZLACGV(I-1, X(I,1), LDX)
				zlacgv( i, X, strideX2, offsetX + i * sx1 );

				// Fortran: CALL ZGEMV('C', I-1, N-I, -ONE, A(1,I+1), LDA, X(I,1), LDX, ONE, A(I,I+1), LDA)
				zgemv( 'C', i, N - i - 1, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa2,
					X, strideX2, offsetX + i * sx1,
					ONE, A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2
				);

				// Unconjugate X(I, 1:I-1)
				zlacgv( i, X, strideX2, offsetX + i * sx1 );

				// Generate elementary reflector G(i) to annihilate A(i, i+2:N-1)
				//
				// Fortran: ALPHA = A(I,I+1)
				// CALL ZLARFG(N-I, ALPHA, A(I, MIN(I+2,N)), LDA, TAUP(I))
				// E(I) = DBLE(ALPHA)
				// A(I,I+1) = ONE
				//
				// alpha is at A(I,I+1) => offsetA + i*sa1 + (i+1)*sa2
				// x starts at A(I, MIN(I+2,N)) => row i, col min(i+2, N-1) (0-based)
				zlarfg( N - i - 1, A, offsetA + i * sa1 + ( i + 1 ) * sa2,
					A, strideA2, offsetA + i * sa1 + Math.min( i + 2, N - 1 ) * sa2,
					TAUP, offsetTAUP + i * stp
				);
				e[ offsetE + i * strideE ] = A[ offsetA + i * sa1 + ( i + 1 ) * sa2 ];
				A[ offsetA + i * sa1 + ( i + 1 ) * sa2 ] = 1.0;
				A[ offsetA + i * sa1 + ( i + 1 ) * sa2 + 1 ] = 0.0;

				// Compute X(i+1:M-1, i)
				//
				// Fortran: CALL ZGEMV('N', M-I, N-I, ONE, A(I+1,I+1), LDA, A(I,I+1), LDA, ZERO, X(I+1,I), 1)
				// A(I+1,I+1) => row i+1, col i+1
				// A(I,I+1) => row i, col i+1, stride=LDA => strideA2
				// X(I+1,I) => row i+1, col i, stride=1 => strideX1
				zgemv( 'N', M - i - 1, N - i - 1, ONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1 + ( i + 1 ) * sa2,
					A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2,
					ZERO, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZGEMV('C', N-I, I, ONE, Y(I+1,1), LDY, A(I,I+1), LDA, ZERO, X(1,I), 1)
				// Y(I+1,1) => row i+1, col 0
				// A(I,I+1) => row i, col i+1, stride=LDA => strideA2
				// X(1,I) => row 0, col i, stride=1 => strideX1
				zgemv( 'C', N - i - 1, i + 1, ONE,
					Y, strideY1, strideY2, offsetY + ( i + 1 ) * sy1,
					A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2,
					ZERO, X, strideX1, offsetX + i * sx2
				);

				// Fortran: CALL ZGEMV('N', M-I, I, -ONE, A(I+1,1), LDA, X(1,I), 1, ONE, X(I+1,I), 1)
				// A(I+1,1) => row i+1, col 0
				// X(1,I) => row 0, col i, stride=1 => strideX1
				// X(I+1,I) => row i+1, col i, stride=1 => strideX1
				zgemv( 'N', M - i - 1, i + 1, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1,
					X, strideX1, offsetX + i * sx2,
					ONE, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZGEMV('N', I-1, N-I, ONE, A(1,I+1), LDA, A(I,I+1), LDA, ZERO, X(1,I), 1)
				// A(1,I+1) => row 0, col i+1
				// A(I,I+1) => row i, col i+1, stride=LDA => strideA2
				// X(1,I) => row 0, col i, stride=1 => strideX1
				zgemv( 'N', i, N - i - 1, ONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa2,
					A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2,
					ZERO, X, strideX1, offsetX + i * sx2
				);

				// Fortran: CALL ZGEMV('N', M-I, I-1, -ONE, X(I+1,1), LDX, X(1,I), 1, ONE, X(I+1,I), 1)
				// X(I+1,1) => row i+1, col 0
				// X(1,I) => row 0, col i, stride=1 => strideX1
				// X(I+1,I) => row i+1, col i, stride=1 => strideX1
				zgemv( 'N', M - i - 1, i, NEGONE,
					X, strideX1, strideX2, offsetX + ( i + 1 ) * sx1,
					X, strideX1, offsetX + i * sx2,
					ONE, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZSCAL(M-I, TAUP(I), X(I+1,I), 1)
				zscal( M - i - 1, TAUP.subarray( offsetTAUP + i * stp, offsetTAUP + i * stp + 2 ),
					X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Unconjugate A(I, I+1:N-1)
				zlacgv( N - i - 1, A, strideA2, offsetA + i * sa1 + ( i + 1 ) * sa2 );
			}
		}
	} else {
		// Reduce to lower bidiagonal form (M < N)
		for ( i = 0; i < nb; i++ ) {
			// Update A(i, i:N-1)
			//
			// Fortran: CALL ZLACGV(N-I+1, A(I,I), LDA)
			zlacgv( N - i, A, strideA2, offsetA + i * sa1 + i * sa2 );

			// Fortran: CALL ZLACGV(I-1, A(I,1), LDA)
			zlacgv( i, A, strideA2, offsetA + i * sa1 );

			// Fortran: CALL ZGEMV('N', N-I+1, I-1, -ONE, Y(I,1), LDY, A(I,1), LDA, ONE, A(I,I), LDA)
			// Y(I,1) => row i, col 0 of Y
			// A(I,1) => row i, col 0, stride=LDA => strideA2
			// A(I,I) => row i, col i, stride=LDA => strideA2
			zgemv( 'N', N - i, i, NEGONE,
				Y, strideY1, strideY2, offsetY + i * sy1,
				A, strideA2, offsetA + i * sa1,
				ONE, A, strideA2, offsetA + i * sa1 + i * sa2
			);

			// Unconjugate A(I, 1:I-1)
			zlacgv( i, A, strideA2, offsetA + i * sa1 );

			// Fortran: CALL ZLACGV(I-1, X(I,1), LDX)
			zlacgv( i, X, strideX2, offsetX + i * sx1 );

			// Fortran: CALL ZGEMV('C', I-1, N-I+1, -ONE, A(1,I), LDA, X(I,1), LDX, ONE, A(I,I), LDA)
			// A(1,I) => row 0, col i
			// X(I,1) => row i, col 0, stride=LDX => strideX2
			// A(I,I) => row i, col i, stride=LDA => strideA2
			zgemv( 'C', i, N - i, NEGONE,
				A, strideA1, strideA2, offsetA + i * sa2,
				X, strideX2, offsetX + i * sx1,
				ONE, A, strideA2, offsetA + i * sa1 + i * sa2
			);

			// Unconjugate X(I, 1:I-1)
			zlacgv( i, X, strideX2, offsetX + i * sx1 );

			// Generate elementary reflector G(i) to annihilate A(i, i+1:N-1)
			//
			// Fortran: ALPHA = A(I,I)
			// CALL ZLARFG(N-I+1, ALPHA, A(I, MIN(I+1,N)), LDA, TAUP(I))
			// D(I) = DBLE(ALPHA)
			//
			// Save A(I,I) before zlarfg (Fortran uses local ALPHA variable)
			ia = offsetA + i * sa1 + i * sa2;
			alphaRe = A[ ia ];
			alphaIm = A[ ia + 1 ];
			alpha[ 0 ] = alphaRe;
			alpha[ 1 ] = alphaIm;
			zlarfg( N - i, alpha, 0,
				A, strideA2, offsetA + i * sa1 + Math.min( i + 1, N - 1 ) * sa2,
				TAUP, offsetTAUP + i * stp
			);
			d[ offsetD + i * strideD ] = alpha[ 0 ];

			if ( i < M - 1 ) {
				// Set A(i,i) = 1 for use as reflector vector
				A[ ia ] = 1.0;
				A[ ia + 1 ] = 0.0;

				// Compute X(i+1:M-1, i)
				//
				// Fortran: CALL ZGEMV('N', M-I, N-I+1, ONE, A(I+1,I), LDA, A(I,I), LDA, ZERO, X(I+1,I), 1)
				// A(I+1,I) => row i+1, col i
				// A(I,I) => row i, col i, stride=LDA => strideA2
				// X(I+1,I) => row i+1, col i, stride=1 => strideX1
				zgemv( 'N', M - i - 1, N - i, ONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1 + i * sa2,
					A, strideA2, offsetA + i * sa1 + i * sa2,
					ZERO, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZGEMV('C', N-I+1, I-1, ONE, Y(I,1), LDY, A(I,I), LDA, ZERO, X(1,I), 1)
				// Y(I,1) => row i, col 0
				// A(I,I) => row i, col i, stride=LDA => strideA2
				// X(1,I) => row 0, col i, stride=1 => strideX1
				zgemv( 'C', N - i, i, ONE,
					Y, strideY1, strideY2, offsetY + i * sy1,
					A, strideA2, offsetA + i * sa1 + i * sa2,
					ZERO, X, strideX1, offsetX + i * sx2
				);

				// Fortran: CALL ZGEMV('N', M-I, I-1, -ONE, A(I+1,1), LDA, X(1,I), 1, ONE, X(I+1,I), 1)
				zgemv( 'N', M - i - 1, i, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1,
					X, strideX1, offsetX + i * sx2,
					ONE, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZGEMV('N', I-1, N-I+1, ONE, A(1,I), LDA, A(I,I), LDA, ZERO, X(1,I), 1)
				// A(1,I) => row 0, col i
				// A(I,I) => row i, col i, stride=LDA => strideA2
				// X(1,I) => row 0, col i, stride=1 => strideX1
				zgemv( 'N', i, N - i, ONE,
					A, strideA1, strideA2, offsetA + i * sa2,
					A, strideA2, offsetA + i * sa1 + i * sa2,
					ZERO, X, strideX1, offsetX + i * sx2
				);

				// Fortran: CALL ZGEMV('N', M-I, I-1, -ONE, X(I+1,1), LDX, X(1,I), 1, ONE, X(I+1,I), 1)
				zgemv( 'N', M - i - 1, i, NEGONE,
					X, strideX1, strideX2, offsetX + ( i + 1 ) * sx1,
					X, strideX1, offsetX + i * sx2,
					ONE, X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Fortran: CALL ZSCAL(M-I, TAUP(I), X(I+1,I), 1)
				zscal( M - i - 1, TAUP.subarray( offsetTAUP + i * stp, offsetTAUP + i * stp + 2 ),
					X, strideX1, offsetX + ( i + 1 ) * sx1 + i * sx2
				);

				// Unconjugate A(I, I:N-1)
				zlacgv( N - i, A, strideA2, offsetA + i * sa1 + i * sa2 );

				// Update A(i+1:M-1, i)
				//
				// Fortran: CALL ZLACGV(I-1, Y(I,1), LDY)
				zlacgv( i, Y, strideY2, offsetY + i * sy1 );

				// Fortran: CALL ZGEMV('N', M-I, I-1, -ONE, A(I+1,1), LDA, Y(I,1), LDY, ONE, A(I+1,I), 1)
				// A(I+1,1) => row i+1, col 0
				// Y(I,1) => row i, col 0, stride=LDY => strideY2
				// A(I+1,I) => row i+1, col i, stride=1 => strideA1
				zgemv( 'N', M - i - 1, i, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1,
					Y, strideY2, offsetY + i * sy1,
					ONE, A, strideA1, offsetA + ( i + 1 ) * sa1 + i * sa2
				);

				// Unconjugate Y(I, 1:I-1)
				zlacgv( i, Y, strideY2, offsetY + i * sy1 );

				// Fortran: CALL ZGEMV('N', M-I, I, -ONE, X(I+1,1), LDX, A(1,I), 1, ONE, A(I+1,I), 1)
				// X(I+1,1) => row i+1, col 0
				// A(1,I) => row 0, col i, stride=1 => strideA1
				// A(I+1,I) => row i+1, col i, stride=1 => strideA1
				zgemv( 'N', M - i - 1, i + 1, NEGONE,
					X, strideX1, strideX2, offsetX + ( i + 1 ) * sx1,
					A, strideA1, offsetA + i * sa2,
					ONE, A, strideA1, offsetA + ( i + 1 ) * sa1 + i * sa2
				);

				// Generate elementary reflector H(i) to annihilate A(i+2:M-1, i)
				//
				// Fortran: ALPHA = A(I+1,I)
				// CALL ZLARFG(M-I, ALPHA, A(MIN(I+2,M),I), 1, TAUQ(I))
				// E(I) = DBLE(ALPHA)
				// A(I+1,I) = ONE
				zlarfg( M - i - 1, A, offsetA + ( i + 1 ) * sa1 + i * sa2,
					A, strideA1, offsetA + Math.min( i + 2, M - 1 ) * sa1 + i * sa2,
					TAUQ, offsetTAUQ + i * stq
				);
				e[ offsetE + i * strideE ] = A[ offsetA + ( i + 1 ) * sa1 + i * sa2 ];
				A[ offsetA + ( i + 1 ) * sa1 + i * sa2 ] = 1.0;
				A[ offsetA + ( i + 1 ) * sa1 + i * sa2 + 1 ] = 0.0;

				// Compute Y(i+1:N-1, i)
				//
				// Fortran: CALL ZGEMV('C', M-I, N-I, ONE, A(I+1,I+1), LDA, A(I+1,I), 1, ZERO, Y(I+1,I), 1)
				// A(I+1,I+1) => row i+1, col i+1
				// A(I+1,I) => row i+1, col i, stride=1 => strideA1
				// Y(I+1,I) => row i+1, col i, stride=1 => strideY1
				zgemv( 'C', M - i - 1, N - i - 1, ONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1 + ( i + 1 ) * sa2,
					A, strideA1, offsetA + ( i + 1 ) * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZGEMV('C', M-I, I-1, ONE, A(I+1,1), LDA, A(I+1,I), 1, ZERO, Y(1,I), 1)
				zgemv( 'C', M - i - 1, i, ONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa1,
					A, strideA1, offsetA + ( i + 1 ) * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + i * sy2
				);

				// Fortran: CALL ZGEMV('N', N-I, I-1, -ONE, Y(I+1,1), LDY, Y(1,I), 1, ONE, Y(I+1,I), 1)
				zgemv( 'N', N - i - 1, i, NEGONE,
					Y, strideY1, strideY2, offsetY + ( i + 1 ) * sy1,
					Y, strideY1, offsetY + i * sy2,
					ONE, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZGEMV('C', M-I, I, ONE, X(I+1,1), LDX, A(I+1,I), 1, ZERO, Y(1,I), 1)
				zgemv( 'C', M - i - 1, i + 1, ONE,
					X, strideX1, strideX2, offsetX + ( i + 1 ) * sx1,
					A, strideA1, offsetA + ( i + 1 ) * sa1 + i * sa2,
					ZERO, Y, strideY1, offsetY + i * sy2
				);

				// Fortran: CALL ZGEMV('C', I, N-I, -ONE, A(1,I+1), LDA, Y(1,I), 1, ONE, Y(I+1,I), 1)
				zgemv( 'C', i + 1, N - i - 1, NEGONE,
					A, strideA1, strideA2, offsetA + ( i + 1 ) * sa2,
					Y, strideY1, offsetY + i * sy2,
					ONE, Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);

				// Fortran: CALL ZSCAL(N-I, TAUQ(I), Y(I+1,I), 1)
				zscal( N - i - 1, TAUQ.subarray( offsetTAUQ + i * stq, offsetTAUQ + i * stq + 2 ),
					Y, strideY1, offsetY + ( i + 1 ) * sy1 + i * sy2
				);
			} else {
				// Unconjugate A(I, I:N-1)
				zlacgv( N - i, A, strideA2, offsetA + i * sa1 + i * sa2 );
			}
		}
	}
}


// EXPORTS //

module.exports = zlabrd;
