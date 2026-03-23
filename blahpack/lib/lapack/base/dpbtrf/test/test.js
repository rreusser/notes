

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dpbtrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpbtrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'dpbtrf: upper_tridiag_5', function t() {
	var tc = findCase( 'upper_tridiag_5' );
	var ab = new Float64Array([
		0.0,  2.0,
		-1.0, 2.0,
		-1.0, 2.0,
		-1.0, 2.0,
		-1.0, 2.0
	]);
	var info = dpbtrf( 'U', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: lower_tridiag_5', function t() {
	var tc = findCase( 'lower_tridiag_5' );
	var ab = new Float64Array([
		2.0,  -1.0,
		2.0,  -1.0,
		2.0,  -1.0,
		2.0,  -1.0,
		2.0,   0.0
	]);
	var info = dpbtrf( 'L', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var ab = new Float64Array([ 99.0 ]);
	var info = dpbtrf( 'U', 0, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpbtrf: n_one', function t() {
	var tc = findCase( 'n_one' );
	var ab = new Float64Array([ 4.0 ]);
	var info = dpbtrf( 'L', 1, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: upper_penta_4', function t() {
	var tc = findCase( 'upper_penta_4' );
	var ab = new Float64Array([
		0.0,  0.0,  4.0,
		0.0, -1.0,  4.0,
		0.5, -1.0,  4.0,
		0.5, -1.0,  4.0
	]);
	var info = dpbtrf( 'U', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: lower_penta_4', function t() {
	var tc = findCase( 'lower_penta_4' );
	var ab = new Float64Array([
		4.0, -1.0,  0.5,
		4.0, -1.0,  0.5,
		4.0, -1.0,  0.0,
		4.0,  0.0,  0.0
	]);
	var info = dpbtrf( 'L', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var ab = new Float64Array([
		1.0,  2.0,
		1.0,  0.0
	]);
	var info = dpbtrf( 'L', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: upper_banded_8', function t() {
	var tc = findCase( 'upper_banded_8' );
	var ab = new Float64Array([
		0.0,  0.0,  6.0,
		0.0, -1.0,  6.0,
		0.5, -1.0,  6.0,
		0.5, -1.0,  6.0,
		0.5, -1.0,  6.0,
		0.5, -1.0,  6.0,
		0.5, -1.0,  6.0,
		0.5, -1.0,  6.0
	]);
	var info = dpbtrf( 'U', 8, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: lower_banded_8', function t() {
	var tc = findCase( 'lower_banded_8' );
	var ab = new Float64Array([
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.5,
		6.0, -1.0,  0.0,
		6.0,  0.0,  0.0
	]);
	var info = dpbtrf( 'L', 8, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtrf: blocked path upper (KD >= 32)', function t() {
	// Create a large enough banded SPD matrix so that NB=32 <= KD triggers blocked path
	var n = 64;
	var kd = 32;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var i;
	var j;
	var d;

	// Build a diagonally dominant SPD banded matrix in upper band storage
	// Diagonal dominance: diag = kd + 1, off-diags = -1 (absolute sum < diag)
	for ( j = 0; j < n; j++ ) {
		// Diagonal at row kd
		ab[ kd + j * ldab ] = kd + 2.0;
		// Off-diagonals
		for ( d = 1; d <= kd; d++ ) {
			if ( j + d < n ) {
				// AB(kd+1-d, j+d) in 0-based: (kd-d) + (j+d)*ldab
				ab[ ( kd - d ) + ( j + d ) * ldab ] = -0.01;
			}
		}
	}

	var info = dpbtrf( 'U', n, kd, ab, 1, ldab, 0 );
	assert.equal( info, 0, 'blocked upper factorization should succeed' );

	// Verify: diagonal elements should be positive (Cholesky factor)
	for ( j = 0; j < n; j++ ) {
		assert.ok( ab[ kd + j * ldab ] > 0, 'diagonal element ' + j + ' should be positive' );
	}
});

test( 'dpbtrf: blocked path lower (KD >= 32)', function t() {
	var n = 64;
	var kd = 32;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var j;
	var d;

	// Build a diagonally dominant SPD banded matrix in lower band storage
	for ( j = 0; j < n; j++ ) {
		// Diagonal at row 0
		ab[ j * ldab ] = kd + 2.0;
		// Off-diagonals
		for ( d = 1; d <= kd; d++ ) {
			if ( j + d < n ) {
				// AB(1+d, j) in 0-based: d + j*ldab
				// Wait - lower storage: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)
				// So AB(d+1, j) in 1-based = AB[d + j*ldab] = A(j+d, j)
				ab[ d + j * ldab ] = -0.01;
			}
		}
	}

	var info = dpbtrf( 'L', n, kd, ab, 1, ldab, 0 );
	assert.equal( info, 0, 'blocked lower factorization should succeed' );

	for ( j = 0; j < n; j++ ) {
		assert.ok( ab[ j * ldab ] > 0, 'diagonal element ' + j + ' should be positive' );
	}
});

test( 'dpbtrf: blocked path upper with i2>0 (KD=48)', function t() {
	// KD=48 > NB=32, so i2 = min(48-32, ...) = 16 > 0
	// Need N large enough so i3 > 0 too: i3 = min(32, N-i-48)
	// With N=128: first block i=0, ib=32, i2=min(16,96)=16, i3=min(32,128-0-48)=32
	var n = 128;
	var kd = 48;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var j;
	var d;

	for ( j = 0; j < n; j++ ) {
		ab[ kd + j * ldab ] = kd + 2.0;
		for ( d = 1; d <= kd; d++ ) {
			if ( j + d < n ) {
				ab[ ( kd - d ) + ( j + d ) * ldab ] = -0.01;
			}
		}
	}

	var info = dpbtrf( 'U', n, kd, ab, 1, ldab, 0 );
	assert.equal( info, 0, 'blocked upper with i2>0 should succeed' );

	for ( j = 0; j < n; j++ ) {
		assert.ok( ab[ kd + j * ldab ] > 0, 'diagonal element ' + j + ' should be positive' );
	}
});

test( 'dpbtrf: blocked path lower with i2>0 (KD=48)', function t() {
	var n = 128;
	var kd = 48;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var j;
	var d;

	for ( j = 0; j < n; j++ ) {
		ab[ j * ldab ] = kd + 2.0;
		for ( d = 1; d <= kd; d++ ) {
			if ( j + d < n ) {
				ab[ d + j * ldab ] = -0.01;
			}
		}
	}

	var info = dpbtrf( 'L', n, kd, ab, 1, ldab, 0 );
	assert.equal( info, 0, 'blocked lower with i2>0 should succeed' );

	for ( j = 0; j < n; j++ ) {
		assert.ok( ab[ j * ldab ] > 0, 'diagonal element ' + j + ' should be positive' );
	}
});

test( 'dpbtrf: blocked path not_posdef upper', function t() {
	// Create a matrix that will fail during the blocked factorization
	var n = 64;
	var kd = 32;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var j;

	// Set diagonal to 1, off-diag to large values so it's not SPD
	for ( j = 0; j < n; j++ ) {
		ab[ kd + j * ldab ] = 1.0;
		if ( j + 1 < n ) {
			ab[ ( kd - 1 ) + ( j + 1 ) * ldab ] = 10.0;
		}
	}

	var info = dpbtrf( 'U', n, kd, ab, 1, ldab, 0 );
	assert.ok( info > 0, 'should return positive info for non-SPD matrix' );
});

test( 'dpbtrf: blocked path not_posdef lower', function t() {
	var n = 64;
	var kd = 32;
	var ldab = kd + 1;
	var ab = new Float64Array( ldab * n );
	var j;

	for ( j = 0; j < n; j++ ) {
		ab[ j * ldab ] = 1.0;
		if ( j + 1 < n ) {
			ab[ 1 + j * ldab ] = 10.0;
		}
	}

	var info = dpbtrf( 'L', n, kd, ab, 1, ldab, 0 );
	assert.ok( info > 0, 'should return positive info for non-SPD matrix' );
});
