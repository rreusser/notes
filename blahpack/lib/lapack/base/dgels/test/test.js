'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgels = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgels.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Extracts a sub-vector from a column-major matrix B stored as Float64Array.
*
* @param {Float64Array} B - matrix stored in column-major order
* @param {integer} LDB - leading dimension of B
* @param {integer} col - column index (0-based)
* @param {integer} len - number of rows to extract
* @returns {Array} extracted values
*/
function extractCol( B, LDB, col, len ) {
	var result = [];
	var i;
	for ( i = 0; i < len; i++ ) {
		result.push( B[ col * LDB + i ] );
	}
	return result;
}


// TESTS //

test( 'dgels: overdetermined 4x2, TRANS=N (least squares)', function t() {
	var tc = findCase( 'overdetermined_4x2' );
	var info;
	var A;
	var B;

	// A = [1 1; 1 2; 1 3; 1 4] (4x2 col-major)
	A = new Float64Array([
		1.0, 1.0, 1.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	]);
	// b = [1; 2; 4; 3]
	B = new Float64Array([
		1.0, 2.0, 4.0, 3.0
	]);

	info = dgels( 'no-transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1] ], tc.x, 1e-14, 'x' );
});

test( 'dgels: underdetermined 2x4, TRANS=N (minimum norm)', function t() {
	var tc = findCase( 'underdetermined_2x4' );
	var info;
	var A;
	var B;

	// A = [1 2 3 4; 5 6 7 8] (2x4 col-major), LDA=2
	A = new Float64Array([
		1.0, 5.0,
		2.0, 6.0,
		3.0, 7.0,
		4.0, 8.0
	]);
	// b = [10; 26], LDB = max(M,N) = 4
	B = new Float64Array([
		10.0, 26.0, 0.0, 0.0
	]);

	info = dgels( 'no-transpose', 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1], B[2], B[3] ], tc.x, 1e-14, 'x' );
});

test( 'dgels: square 3x3, TRANS=N', function t() {
	var tc = findCase( 'square_3x3' );
	var info;
	var A;
	var B;

	// A = [5 1 1; 1 5 1; 1 1 5] (col-major)
	A = new Float64Array([
		5.0, 1.0, 1.0,
		1.0, 5.0, 1.0,
		1.0, 1.0, 5.0
	]);
	B = new Float64Array([
		7.0, 7.0, 7.0
	]);

	info = dgels( 'no-transpose', 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1], B[2] ], tc.x, 1e-14, 'x' );
});

test( 'dgels: TRANS=T, M < N (least squares of A^T * x = b)', function t() {
	var tc = findCase( 'transpose_mlt_n_ls' );
	var info;
	var A;
	var B;

	// A = [1 2 3 4; 5 6 7 8] (2x4 col-major)
	// TRANS='T': least squares min || b - A^T * x ||
	// b has 4 entries, x has 2 entries (M=2)
	A = new Float64Array([
		1.0, 5.0,
		2.0, 6.0,
		3.0, 7.0,
		4.0, 8.0
	]);
	B = new Float64Array([
		1.0, 2.0, 4.0, 3.0
	]);

	info = dgels( 'transpose', 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1] ], tc.x, 1e-14, 'x' );
});

test( 'dgels: TRANS=T, M >= N (minimum norm of A^T * x = b)', function t() {
	var tc = findCase( 'transpose_mge_n_minnorm' );
	var info;
	var A;
	var B;

	// A = [1 1; 1 2; 1 3; 1 4] (4x2 col-major)
	// TRANS='T': min || x || s.t. A^T * x = b
	// b has 2 entries, x has 4 entries (M=4)
	A = new Float64Array([
		1.0, 1.0, 1.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	]);
	B = new Float64Array([
		10.0, 30.0, 0.0, 0.0
	]);

	info = dgels( 'transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1], B[2], B[3] ], tc.x, 1e-14, 'x' );
});

test( 'dgels: multiple RHS, overdetermined 4x2', function t() {
	var tc = findCase( 'multi_rhs_overdetermined' );
	var info;
	var A;
	var B;

	// A = [2 1; 0 2; 1 1; 1 0] (4x2 col-major)
	A = new Float64Array([
		2.0, 0.0, 1.0, 1.0,
		1.0, 2.0, 1.0, 0.0
	]);
	// B = [3 5; 2 4; 2 3; 1 2] (4x2 col-major, LDB=4)
	B = new Float64Array([
		3.0, 2.0, 2.0, 1.0,
		5.0, 4.0, 3.0, 2.0
	]);

	info = dgels( 'no-transpose', 4, 2, 2, A, 1, 4, 0, B, 1, 4, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractCol( B, 4, 0, 2 ), tc.x1, 1e-14, 'x1' );
	assertArrayClose( extractCol( B, 4, 1, 2 ), tc.x2, 1e-14, 'x2' );
});

test( 'dgels: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var info;
	var A;
	var B;

	A = new Float64Array( [ 1.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0 ] );

	info = dgels( 'no-transpose', 3, 0, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info, 'info' );
	// B should be zeroed out (dlaset with max(M,N)=3 rows, NRHS=1 columns)
	assert.equal( B[0], 0.0, 'B[0] zeroed' );
	assert.equal( B[1], 0.0, 'B[1] zeroed' );
	assert.equal( B[2], 0.0, 'B[2] zeroed' );
});

test( 'dgels: M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var info;
	var A;
	var B;

	A = new Float64Array( [ 1.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0 ] );

	info = dgels( 'no-transpose', 0, 3, 1, A, 1, 1, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info, 'info' );
	// B should be zeroed out
	assert.equal( B[0], 0.0, 'B[0] zeroed' );
	assert.equal( B[1], 0.0, 'B[1] zeroed' );
	assert.equal( B[2], 0.0, 'B[2] zeroed' );
});

test( 'dgels: NRHS=0 quick return', function t() {
	var tc = findCase( 'nrhs_zero' );
	var info;
	var A;
	var B;

	A = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	B = new Float64Array( 1 );

	info = dgels( 'no-transpose', 2, 2, 0, A, 1, 2, 0, B, 1, 2, 0 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dgels: larger overdetermined 6x3', function t() {
	var tc = findCase( 'overdetermined_6x3' );
	var info;
	var A;
	var B;

	// A is 6x3 col-major, diagonally dominant
	A = new Float64Array([
		10.0, 1.0, 1.0, 1.0, 1.0, 1.0,   // column 1
		1.0, 10.0, 1.0, 1.0, 1.0, 1.0,    // column 2
		1.0, 1.0, 10.0, 1.0, 1.0, 1.0     // column 3
	]);
	// b = A * [1; 2; 3]
	B = new Float64Array([
		15.0, 24.0, 33.0, 6.0, 6.0, 6.0
	]);

	info = dgels( 'no-transpose', 6, 3, 1, A, 1, 6, 0, B, 1, 6, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( [ B[0], B[1], B[2] ], tc.x, 1e-13, 'x' );
});

test( 'dgels: mathematical property - normal equations for overdetermined', function t() {
	var info;
	var Acopy;
	var A;
	var B;
	var bOrig;
	var x;
	var r;
	var AtR;
	var i;
	var j;
	var M;
	var N;

	M = 4;
	N = 2;

	// A = [1 1; 1 2; 1 3; 1 4]
	Acopy = new Float64Array([
		1.0, 1.0, 1.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	]);
	A = new Float64Array( Acopy );
	B = new Float64Array([
		1.0, 2.0, 4.0, 3.0
	]);

	info = dgels( 'no-transpose', M, N, 1, A, 1, M, 0, B, 1, M, 0 );
	assert.equal( info, 0, 'info' );

	// The solution x is in B[0..N-1]. Compute residual r = b - Acopy*x
	// Then verify A^T * r approx 0 (normal equations)
	r = new Float64Array( M );
	bOrig = [ 1.0, 2.0, 4.0, 3.0 ];
	x = [ B[0], B[1] ];

	for ( i = 0; i < M; i++ ) {
		r[i] = bOrig[i];
		for ( j = 0; j < N; j++ ) {
			r[i] -= Acopy[ j * M + i ] * x[j];
		}
	}

	// A^T * r should be approximately 0
	AtR = new Float64Array( N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			AtR[j] += Acopy[ j * M + i ] * r[i];
		}
	}

	for ( j = 0; j < N; j++ ) {
		assert.ok( Math.abs( AtR[j] ) < 1e-12,
			'Normal equation residual A^T*r[' + j + '] = ' + AtR[j] + ' should be ~0' );
	}
});

test( 'dgels: mathematical property - minimum norm for underdetermined', function t() {
	var info;
	var Acopy;
	var A;
	var B;
	var bOrig;
	var Ax;
	var M;
	var N;
	var i;
	var j;

	M = 2;
	N = 4;

	// A = [1 2 3 4; 5 6 7 8], b = [10; 26]
	Acopy = new Float64Array([
		1.0, 5.0,
		2.0, 6.0,
		3.0, 7.0,
		4.0, 8.0
	]);
	A = new Float64Array( Acopy );
	B = new Float64Array([
		10.0, 26.0, 0.0, 0.0
	]);

	info = dgels( 'no-transpose', M, N, 1, A, 1, M, 0, B, 1, N, 0 );
	assert.equal( info, 0, 'info' );

	// Verify A*x = b
	bOrig = [ 10.0, 26.0 ];
	Ax = new Float64Array( M );
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			Ax[i] += Acopy[ j * M + i ] * B[j];
		}
	}

	for ( i = 0; i < M; i++ ) {
		assertClose( Ax[i], bOrig[i], 1e-13, 'A*x[' + i + ']' );
	}
});

test( 'dgels: matrix with exact zero diagonal in R returns info > 0', function t() {
	var info;
	var A;
	var B;

	// A = [1 0; 0 0] has exact zero at R(2,2) after QR
	A = new Float64Array([
		1.0, 0.0,
		0.0, 0.0
	]);
	B = new Float64Array([
		1.0, 1.0
	]);

	info = dgels( 'no-transpose', 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0 );
	assert.ok( info > 0, 'info should be > 0 for matrix with zero on diagonal, got ' + info );
});

test( 'dgels: all-zero A returns zero solution', function t() {
	var info;
	var A;
	var B;

	A = new Float64Array( 4 ); // 2x2 all zeros
	B = new Float64Array([ 1.0, 2.0 ]);

	info = dgels( 'no-transpose', 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( B[0], 0.0, 'B[0] zeroed for all-zero A' );
	assert.equal( B[1], 0.0, 'B[1] zeroed for all-zero A' );
});

test( 'dgels: tiny A triggers upscaling (iascl=1)', function t() {
	var tc = findCase( 'overdetermined_4x2' );
	var info;
	var A;
	var B;
	var scale;

	scale = 1e-300;

	// Same system as overdetermined_4x2 but scaled by tiny factor
	A = new Float64Array([
		1.0 * scale, 1.0 * scale, 1.0 * scale, 1.0 * scale,
		1.0 * scale, 2.0 * scale, 3.0 * scale, 4.0 * scale
	]);
	B = new Float64Array([
		1.0 * scale, 2.0 * scale, 4.0 * scale, 3.0 * scale
	]);

	info = dgels( 'no-transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0 );
	assert.equal( info, 0, 'info' );
	// Solution should match the unscaled case (scaling is transparent)
	assertArrayClose( [ B[0], B[1] ], tc.x, 1e-12, 'x' );
});

test( 'dgels: huge A triggers downscaling (iascl=2)', function t() {
	var tc = findCase( 'overdetermined_4x2' );
	var info;
	var A;
	var B;
	var scale;

	scale = 1e300;

	// Same system as overdetermined_4x2 but scaled by huge factor
	A = new Float64Array([
		1.0 * scale, 1.0 * scale, 1.0 * scale, 1.0 * scale,
		1.0 * scale, 2.0 * scale, 3.0 * scale, 4.0 * scale
	]);
	B = new Float64Array([
		1.0 * scale, 2.0 * scale, 4.0 * scale, 3.0 * scale
	]);

	info = dgels( 'no-transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( [ B[0], B[1] ], tc.x, 1e-12, 'x' );
});

test( 'dgels: tiny B triggers upscaling (ibscl=1)', function t() {
	var info;
	var Acopy;
	var A;
	var B;
	var bOrig;
	var scale;
	var x;
	var r;
	var AtR;
	var M;
	var N;
	var i;
	var j;

	M = 4;
	N = 2;
	scale = 1e-300;

	// Normal A, but tiny b
	Acopy = new Float64Array([
		1.0, 1.0, 1.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	]);
	A = new Float64Array( Acopy );
	bOrig = [ 1.0 * scale, 2.0 * scale, 4.0 * scale, 3.0 * scale ];
	B = new Float64Array( bOrig );

	info = dgels( 'no-transpose', M, N, 1, A, 1, M, 0, B, 1, M, 0 );
	assert.equal( info, 0, 'info' );

	// Verify normal equations
	x = [ B[0], B[1] ];
	r = new Float64Array( M );
	for ( i = 0; i < M; i++ ) {
		r[i] = bOrig[i];
		for ( j = 0; j < N; j++ ) {
			r[i] -= Acopy[ j * M + i ] * x[j];
		}
	}
	AtR = new Float64Array( N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			AtR[j] += Acopy[ j * M + i ] * r[i];
		}
	}
	for ( j = 0; j < N; j++ ) {
		assert.ok( Math.abs( AtR[j] ) < 1e-10 * scale,
			'Normal equation A^T*r[' + j + '] = ' + AtR[j] + ' should be ~0' );
	}
});

test( 'dgels: huge B triggers downscaling (ibscl=2)', function t() {
	var info;
	var Acopy;
	var A;
	var B;
	var bOrig;
	var scale;
	var x;
	var r;
	var AtR;
	var M;
	var N;
	var i;
	var j;

	M = 4;
	N = 2;
	scale = 1e300;

	// Normal A, but huge b
	Acopy = new Float64Array([
		1.0, 1.0, 1.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	]);
	A = new Float64Array( Acopy );
	bOrig = [ 1.0 * scale, 2.0 * scale, 4.0 * scale, 3.0 * scale ];
	B = new Float64Array( bOrig );

	info = dgels( 'no-transpose', M, N, 1, A, 1, M, 0, B, 1, M, 0 );
	assert.equal( info, 0, 'info' );

	// Verify normal equations
	x = [ B[0], B[1] ];
	r = new Float64Array( M );
	for ( i = 0; i < M; i++ ) {
		r[i] = bOrig[i];
		for ( j = 0; j < N; j++ ) {
			r[i] -= Acopy[ j * M + i ] * x[j];
		}
	}
	AtR = new Float64Array( N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			AtR[j] += Acopy[ j * M + i ] * r[i];
		}
	}
	for ( j = 0; j < N; j++ ) {
		assert.ok( Math.abs( AtR[j] ) < 1e-10 * scale,
			'Normal equation A^T*r[' + j + '] = ' + AtR[j] + ' should be ~0' );
	}
});

test( 'dgels: singular in underdetermined TRANS=N returns info > 0', function t() {
	var info;
	var A;
	var B;

	// A = [1 0 0 0; 0 0 0 0] (2x4, rank 1, L(2,2)=0 after LQ)
	A = new Float64Array([
		1.0, 0.0,
		0.0, 0.0,
		0.0, 0.0,
		0.0, 0.0
	]);
	B = new Float64Array([
		1.0, 1.0, 0.0, 0.0
	]);

	info = dgels( 'no-transpose', 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0 );
	assert.ok( info > 0, 'info should be > 0 for singular L in underdetermined, got ' + info );
});

test( 'dgels: singular in TRANS=T M>=N (dtrtrs on R^T) returns info > 0', function t() {
	var info;
	var A;
	var B;

	// A = [1 0; 0 0; 0 0; 0 0] (4x2, R(2,2) = 0 after QR)
	A = new Float64Array([
		1.0, 0.0, 0.0, 0.0,
		0.0, 0.0, 0.0, 0.0
	]);
	B = new Float64Array([
		1.0, 1.0, 0.0, 0.0
	]);

	info = dgels( 'transpose', 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0 );
	assert.ok( info > 0, 'info should be > 0 for singular R in TRANS=T M>=N, got ' + info );
});

test( 'dgels: singular in TRANS=T M<N (dtrtrs on L^T) returns info > 0', function t() {
	var info;
	var A;
	var B;

	// A = [1 0 0 0; 0 0 0 0] (2x4, L(2,2) = 0 after LQ)
	A = new Float64Array([
		1.0, 0.0,
		0.0, 0.0,
		0.0, 0.0,
		0.0, 0.0
	]);
	B = new Float64Array([
		1.0, 1.0, 0.0, 0.0
	]);

	info = dgels( 'transpose', 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0 );
	assert.ok( info > 0, 'info should be > 0 for singular L in TRANS=T M<N, got ' + info );
});
