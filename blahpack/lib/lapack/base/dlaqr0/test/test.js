/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlaqr0 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlaqr0.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

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
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Sort eigenvalues by real part, then imaginary part, for comparison.
*/
function sortedEigs( wr, wi ) {
	var eigs = [];
	var i;
	for ( i = 0; i < wr.length; i++ ) {
		eigs.push({
			're': wr[ i ],
			'im': wi[ i ]
		});
	}
	eigs.sort( function cmp( a, b ) {
		if ( a.re !== b.re ) {
			return a.re - b.re;
		}
		return a.im - b.im;
	});
	return eigs;
}

/**
* AssertEigenvaluesClose.
*
* @private
* @param {*} wrActual - wrActual
* @param {*} wiActual - wiActual
* @param {*} wrExpected - wrExpected
* @param {*} wiExpected - wiExpected
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertEigenvaluesClose( wrActual, wiActual, wrExpected, wiExpected, tol, msg ) { // eslint-disable-line max-len
	var expected = sortedEigs( wrExpected, wiExpected );
	var actual = sortedEigs( wrActual, wiActual );
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ].re, expected[ i ].re, tol, msg + '.re[' + i + ']' );
		assertClose( actual[ i ].im, expected[ i ].im, tol, msg + '.im[' + i + ']' );
	}
}

/**
* Build a column-major N x N matrix from a flat array (Fortran order with LDH=MAXN).
* For our tests we use LDH = N (dense).
*/
function buildHessenberg( N, initFn ) {
	var H = new Float64Array( N * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			H[ i + j * N ] = initFn( i + 1, j + 1 );
		}
	}
	return H;
}

/**
* Identity.
*
* @private
* @param {*} N - N
* @returns {*} result
*/
function identity( N ) {
	var Z = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		Z[ i + i * N ] = 1.0;
	}
	return Z;
}

/**
* ExtractEigs.
*
* @private
* @param {*} WR - WR
* @param {*} WI - WI
* @param {*} N - N
* @param {*} offset - offset
* @returns {*} result
*/
function extractEigs( WR, WI, N, offset ) {
	var wr = [];
	var wi = [];
	var i;
	for ( i = 0; i < N; i++ ) {
		wr.push( WR[ offset + i ] );
		wi.push( WI[ offset + i ] );
	}
	return {
		'wr': wr,
		'wi': wi
	};
}


// TESTS //

test( 'dlaqr0: n_eq_0', function t() {
	var WORK;
	var info;
	var WR;
	var WI;
	var H;
	var Z;

	WORK = new Float64Array( 10 );
	WR = new Float64Array( 1 );
	WI = new Float64Array( 1 );
	H = new Float64Array( 1 );
	Z = new Float64Array( 1 );
	info = dlaqr0( true, true, 0, 1, 0, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, 1, 0, Z, 1, 1, 0, WORK, 1, 0, 10 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
});

test( 'dlaqr0: n_eq_1', function t() {
	var WORK;
	var info;
	var tc;
	var WR;
	var WI;
	var H;
	var Z;

	tc = findCase( 'n_eq_1' );
	WORK = new Float64Array( 10 );
	WR = new Float64Array( 1 );
	WI = new Float64Array( 1 );
	H = new Float64Array( [ 3.5 ] );
	Z = new Float64Array( [ 1.0 ] );
	info = dlaqr0( true, true, 1, 1, 1, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, 1, 1, 0, WORK, 1, 0, 10 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( WR[ 0 ], tc.wr1, 1e-14, 'wr1' );
	assertClose( WI[ 0 ], tc.wi1, 1e-14, 'wi1' );
	assertClose( H[ 0 ], tc.h11, 1e-14, 'h11' );
});

test( 'dlaqr0: n_eq_2', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var Z;

	tc = findCase( 'n_eq_2' );
	N = 2;
	WORK = new Float64Array( 20 );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = new Float64Array( [ 1.0, 3.0, 2.0, 4.0 ] );
	Z = identity( N );
	info = dlaqr0( true, true, N, 1, 2, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 2, Z, 1, N, 0, WORK, 1, 0, 20 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-12, 'eigenvalues' );
});

test( 'dlaqr0: hess_6x6', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var i;
	var H;
	var Z;

	tc = findCase( 'hess_6x6' );
	N = 6;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( j >= i - 1 ) {
			return 1.0 / ( i + j );
		}
		return 0.0;
	});
	for ( i = 2; i <= N; i++ ) {
		H[ ( i - 1 ) + ( i - 2 ) * N ] = 0.5;
	}
	Z = identity( N );
	info = dlaqr0( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-10, 'eigenvalues' );
});

test( 'dlaqr0: hess_6x6_eigonly', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var Z;
	var i;
	var H;

	tc = findCase( 'hess_6x6_eigonly' );
	N = 6;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	Z = new Float64Array( 1 );
	H = buildHessenberg( N, function init( i, j ) {
		if ( j >= i - 1 ) {
			return 1.0 / ( i + j );
		}
		return 0.0;
	});
	for ( i = 2; i <= N; i++ ) {
		H[ ( i - 1 ) + ( i - 2 ) * N ] = 0.5;
	}
	info = dlaqr0( false, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, 1, 1, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-10, 'eigenvalues' );
});

test( 'dlaqr0: ilo_eq_ihi', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var Z;

	tc = findCase( 'ilo_eq_ihi' );
	N = 4;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = new Float64Array( N * N );
	H[ 0 + 0 * N ] = 1.0;
	H[ 1 + 1 * N ] = 2.0;
	H[ 2 + 2 * N ] = 3.0;
	H[ 3 + 3 * N ] = 4.0;
	H[ 0 + 1 * N ] = 0.5;
	H[ 1 + 2 * N ] = 0.5;
	H[ 2 + 3 * N ] = 0.5;
	Z = identity( N );
	info = dlaqr0( true, true, N, 2, 2, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 4, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assertClose( WR[ 1 ], 2.0, 1e-14, 'wr[ilo]' );
	assertClose( WI[ 1 ], 0.0, 1e-14, 'wi[ilo]' );
});

test( 'dlaqr0: hess_15x15', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var Z;

	tc = findCase( 'hess_15x15' );
	N = 15;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return i * 2.0;
		}
		if ( j === i + 1 ) {
			return 1.0;
		}
		if ( j === i - 1 ) {
			return 0.3;
		}
		if ( j > i + 1 ) {
			return 0.1 / ( j - i );
		}
		return 0.0;
	});
	Z = identity( N );
	info = dlaqr0( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-8, 'eigenvalues' );
});

test( 'dlaqr0: hess_16x16', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var Z;

	tc = findCase( 'hess_16x16' );
	N = 16;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return i * 3.0;
		}
		if ( j === i + 1 ) {
			return 2.0;
		}
		if ( j === i - 1 ) {
			return 1.0;
		}
		if ( j > i + 1 ) {
			return 0.05 / ( j - i );
		}
		return 0.0;
	});
	Z = identity( N );
	info = dlaqr0( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-8, 'eigenvalues' );
});

test( 'dlaqr0: hess_20x20', function t() {
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var Z;

	tc = findCase( 'hess_20x20' );
	N = 20;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return i * 2.5 + 0.1 * ( ( i * 7 ) % 11 );
		}
		if ( j === i + 1 ) {
			return 1.5;
		}
		if ( j === i - 1 ) {
			return 0.8;
		}
		if ( j > i + 1 ) {
			return 0.02 / ( j - i );
		}
		return 0.0;
	});
	Z = identity( N );
	info = dlaqr0( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	eigs = extractEigs( WR, WI, N, 0 );
	assertEigenvaluesClose( eigs.wr, eigs.wi, tc.wr, tc.wi, 1e-4, 'eigenvalues' );
});

test( 'dlaqr0: hess_80x80 (exercises dlaqr3/dlaqr5 path, property-based)', function t() { // eslint-disable-line max-len
	var WORK;
	var info;
	var WR;
	var WI;
	var N;
	var i;
	var j;
	var H;
	var Z;

	N = 80;
	WORK = new Float64Array( 100000 );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	Z = identity( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return i * 3.0;
		}
		if ( j === i - 1 ) {
			return 0.1;
		}
		if ( j === i + 1 ) {
			return 0.05;
		}
		return 0.0;
	});
	info = dlaqr0( false, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, 1, 1, 0, WORK, 1, 0, 100000 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info (converged)' );
	for ( i = 0; i < N; i++ ) {
		assert.ok( isFinite( WR[ i ] ), 'WR[' + i + '] is finite' );
		assert.ok( isFinite( WI[ i ] ), 'WI[' + i + '] is finite' );
	}
});

test( 'dlaqr0: hess_80x80 eigenvalues only (property-based)', function t() {
	var WORK;
	var info;
	var WR;
	var WI;
	var N;
	var Z;
	var i;
	var H;

	N = 80;
	WORK = new Float64Array( 100000 );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	Z = new Float64Array( 1 );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return i * 3.0;
		}
		if ( j === i - 1 ) {
			return 0.1;
		}
		if ( j === i + 1 ) {
			return 0.05;
		}
		return 0.0;
	});
	info = dlaqr0( false, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, 1, 1, 0, WORK, 1, 0, 100000 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info (converged)' );
	for ( i = 0; i < N; i++ ) {
		assert.ok( isFinite( WR[ i ] ), 'WR[' + i + '] is finite' );
	}
});

test( 'dlaqr0: hess_40x40 with complex eigenvalues (property-based)', function t() { // eslint-disable-line max-len
	var WORK;
	var info;
	var WR;
	var WI;
	var N;
	var i;
	var H;
	var Z;

	N = 40;
	WORK = new Float64Array( 100000 );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	Z = identity( N );
	H = buildHessenberg( N, function init( i, j ) {
		if ( i === j ) {
			return ( i % 5 ) * 3.0 + 1.0;
		}
		if ( j === i - 1 ) {
			return 2.0;
		}
		if ( j > i && j <= i + 4 ) {
			return 0.5 / ( j - i );
		}
		return 0.0;
	});
	info = dlaqr0( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0, WORK, 1, 0, 100000 ); // eslint-disable-line max-len
	assert.ok( info >= 0, 'info is non-negative' );
	if ( info === 0 ) {
		var hasComplex = false;
		for ( i = 0; i < N; i++ ) {
			if ( Math.abs( WI[ i ] ) > 1e-10 ) {
				hasComplex = true;
				break;
			}
		}
		assert.ok( hasComplex, 'should have complex eigenvalue pairs' );
	}
});

test( 'dlaqr0: partial_block', function t() {
	var wrExpActive;
	var wiExpActive;
	var wrActive;
	var wiActive;
	var WORK;
	var info;
	var eigs;
	var tc;
	var WR;
	var WI;
	var N;
	var H;
	var i;
	var Z;

	tc = findCase( 'partial_block' );
	N = 10;
	WORK = new Float64Array( 10 * N );
	WR = new Float64Array( N );
	WI = new Float64Array( N );
	H = new Float64Array( N * N );
	H[ 0 + 0 * N ] = 10.0;
	H[ 1 + 1 * N ] = 20.0;
	H[ 8 + 8 * N ] = 90.0;
	H[ 9 + 9 * N ] = 100.0;
	H[ 0 + 1 * N ] = 1.0;
	H[ 8 + 9 * N ] = 1.0;
	for ( i = 3; i <= 8; i++ ) {
		H[ ( i - 1 ) + ( i - 1 ) * N ] = i * 5.0;
		if ( i < 8 ) {
			H[ ( i - 1 ) + i * N ] = 2.0;
		}
		if ( i > 3 ) {
			H[ i - 1 + ( i - 2 ) * N ] = 1.0;  // subdiag: H(i, i-1)
		}
	}
	H[ 2 + 4 * N ] = 0.3;
	H[ 3 + 5 * N ] = 0.2;
	H[ 4 + 6 * N ] = 0.1;
	H[ 1 + 2 * N ] = 0.5;
	H[ 7 + 8 * N ] = 0.5;
	Z = identity( N );
	info = dlaqr0( true, true, N, 3, 8, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, 10, Z, 1, N, 0, WORK, 1, 0, 10 * N ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	wrActive = [];
	wiActive = [];
	wrExpActive = [];
	wiExpActive = [];
	for ( i = 2; i < 8; i++ ) {
		wrActive.push( WR[ i ] );
		wiActive.push( WI[ i ] );
		wrExpActive.push( tc.wr[ i ] );
		wiExpActive.push( tc.wi[ i ] );
	}
	assertEigenvaluesClose( wrActive, wiActive, wrExpActive, wiExpActive, 1e-8, 'eigenvalues' ); // eslint-disable-line max-len
});
