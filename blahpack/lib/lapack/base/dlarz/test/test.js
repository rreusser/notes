/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var format = require( '@stdlib/string/format' );
var Float64Array = require( '@stdlib/array/float64' );
var dlarz = require( './../lib/dlarz.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarz.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
});

var TOL = 1e-12;


// FUNCTIONS //

/**
* Returns a fixture entry by name.
*
* @private
* @param {string} name - fixture name
* @throws {Error} if the fixture name is unknown
* @returns {Object} fixture entry
*/
function findCase( name ) {
	var tc = fixture.find( function find( t ) {
		return t.name === name;
	});
	if ( !tc ) {
		throw new Error( format( 'unknown fixture: `%s`.', name ) );
	}
	return tc;
}

/**
* Returns the initial C matrix and other inputs for a named test case matching the Fortran driver.
*
* @private
* @param {string} name - case name
* @throws {Error} if the case name is unknown
* @returns {Object} spec with side, M, N, l, v, tau, C (col-major M*N)
*/
function buildCase( name ) {
	var C;
	switch ( name ) {
	case 'left_4x4_l2':
		// M=4, N=4, side='L', l=2, v=[0.5,0.25], tau=1.5
		C = new Float64Array([
			1.0,
			5.0,
			9.0,
			13.0,   // column 1
			2.0,
			6.0,
			10.0,
			14.0,  // column 2
			3.0,
			7.0,
			11.0,
			15.0,  // column 3
			4.0,
			8.0,
			12.0,
			16.0   // column 4
		]);
		return {
			'side': 'left',
			'M': 4,
			'N': 4,
			'l': 2,
			'v': new Float64Array( [ 0.5, 0.25 ] ),
			'tau': 1.5,
			'C': C
		};
	case 'right_4x4_l2':
		C = new Float64Array([
			1.0,
			5.0,
			9.0,
			13.0,
			2.0,
			6.0,
			10.0,
			14.0,
			3.0,
			7.0,
			11.0,
			15.0,
			4.0,
			8.0,
			12.0,
			16.0
		]);
		return {
			'side': 'right',
			'M': 4,
			'N': 4,
			'l': 2,
			'v': new Float64Array( [ 0.5, 0.25 ] ),
			'tau': 1.5,
			'C': C
		};
	case 'tau_zero':
		C = new Float64Array([
			1.0,
			4.0,
			7.0,
			2.0,
			5.0,
			8.0,
			3.0,
			6.0,
			9.0
		]);
		return {
			'side': 'left',
			'M': 3,
			'N': 3,
			'l': 1,
			'v': new Float64Array( [ 1.0 ] ),
			'tau': 0.0,
			'C': C
		};
	case 'left_l0':
		C = new Float64Array([
			1.0,
			4.0,
			7.0,
			2.0,
			5.0,
			8.0,
			3.0,
			6.0,
			9.0
		]);
		return {
			'side': 'left',
			'M': 3,
			'N': 3,
			'l': 0,
			'v': new Float64Array( [ 0.0 ] ),
			'tau': 0.5,
			'C': C
		};
	case 'left_5x3_l3':
		// M=5, N=3 — Fortran fills C(1..5, 1..3)
		C = new Float64Array([
			1.0,
			4.0,
			7.0,
			10.0,
			13.0,
			2.0,
			5.0,
			8.0,
			11.0,
			14.0,
			3.0,
			6.0,
			9.0,
			12.0,
			15.0
		]);
		return {
			'side': 'left',
			'M': 5,
			'N': 3,
			'l': 3,
			'v': new Float64Array( [ 0.1, 0.2, 0.3 ] ),
			'tau': 2.0,
			'C': C
		};
	case 'right_3x5_l3':
		// M=3, N=5
		C = new Float64Array([
			1.0,
			6.0,
			11.0,
			2.0,
			7.0,
			12.0,
			3.0,
			8.0,
			13.0,
			4.0,
			9.0,
			14.0,
			5.0,
			10.0,
			15.0
		]);
		return {
			'side': 'right',
			'M': 3,
			'N': 5,
			'l': 3,
			'v': new Float64Array( [ 0.3, 0.2, 0.1 ] ),
			'tau': -0.5,
			'C': C
		};
	default:
		throw new Error( format( 'unknown case: `%s`.', name ) );
	}
}

/**
* Converts a column-major packed matrix (LD=M) to a row-major packed.
* matrix (LD=N) of the same logical M x N values.
*
* @private
* @param {Float64Array} col - column-major packed M*N
* @param {integer} M - number of rows
* @param {integer} N - number of columns
* @returns {Float64Array} row-major packed M*N
*/
function colToRow( col, M, N ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			out[ ( i * N ) + j ] = col[ ( j * M ) + i ];
		}
	}
	return out;
}

/**
* Asserts that two Float64Arrays agree within TOL.
*
* @private
* @param {Float64Array} actual - computed values
* @param {ArrayLikeObject} expected - expected values
* @param {string} label - label for failure messages
*/
function expectClose( actual, expected, label ) {
	var diff;
	var i;
	assert.equal( actual.length, expected.length, format( '%s: length mismatch', label ) );
	for ( i = 0; i < actual.length; i++ ) {
		diff = Math.abs( actual[ i ] - expected[ i ] );
		assert.ok( diff <= TOL, format( '%s[%d]: expected %s, got %s (diff %s)', label, i, expected[ i ], actual[ i ], diff ) );
	}
}


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarz, 'function', 'is a function' );
});

// Fixture-driven column-major tests
fixture.forEach( function each( tc ) {
	test( 'dlarz (column-major): ' + tc.name, function t() {
		var inputs = buildCase( tc.name );
		var WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
		var LDC = inputs.M; // packed column-major
		dlarz( 'column-major', inputs.side, inputs.M, inputs.N, inputs.l, inputs.v, 1, inputs.tau, inputs.C, LDC, WORK, 1 );
		expectClose( inputs.C, tc.C, tc.name + ' col-major' );
	});
});

// Fixture-driven row-major tests
fixture.forEach( function each( tc ) {
	test( 'dlarz (row-major): ' + tc.name, function t() {
		var expectedRow;
		var inputs;
		var rowC;
		var WORK;
		var LDC;
		inputs = buildCase( tc.name );
		expectedRow = colToRow( new Float64Array( tc.C ), inputs.M, inputs.N );
		rowC = colToRow( inputs.C, inputs.M, inputs.N );
		WORK = new Float64Array( Math.max( inputs.M, inputs.N ) );
		LDC = inputs.N; // packed row-major
		dlarz( 'row-major', inputs.side, inputs.M, inputs.N, inputs.l, inputs.v, 1, inputs.tau, rowC, LDC, WORK, 1 );
		expectClose( rowC, expectedRow, tc.name + ' row-major' );
	});
});

// Argument validation

test( 'dlarz throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		dlarz( 'invalid', 'left', 2, 2, 1, new Float64Array( 2 ), 1, 1.0, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlarz throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		dlarz( 'column-major', 'nope', 2, 2, 1, new Float64Array( 2 ), 1, 1.0, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'dlarz throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		dlarz( 'column-major', 'left', -1, 2, 1, new Float64Array( 2 ), 1, 1.0, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlarz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		dlarz( 'column-major', 'left', 2, -1, 1, new Float64Array( 2 ), 1, 1.0, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlarz throws RangeError when LDC < max(1,M) for column-major', function t() {
	assert.throws( function throws() {
		dlarz( 'column-major', 'left', 4, 4, 2, new Float64Array( [ 0.5, 0.25 ] ), 1, 1.5, new Float64Array( 16 ), 3, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlarz throws RangeError when LDC < max(1,N) for row-major', function t() {
	assert.throws( function throws() {
		dlarz( 'row-major', 'left', 4, 4, 2, new Float64Array( [ 0.5, 0.25 ] ), 1, 1.5, new Float64Array( 16 ), 3, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'dlarz supports LDC > M (padded column-major)', function t() {
	var expected;
	var inputs = buildCase( 'left_4x4_l2' );
	var LDC = 6;
	var C = new Float64Array( LDC * 4 );
	var i;
	var j;
	for ( j = 0; j < 4; j++ ) {
		for ( i = 0; i < 4; i++ ) {
			C[ ( j * LDC ) + i ] = inputs.C[ ( j * 4 ) + i ];
		}
	}
	dlarz( 'column-major', inputs.side, inputs.M, inputs.N, inputs.l, inputs.v, 1, inputs.tau, C, LDC, new Float64Array( 4 ), 1 );
	expected = findCase( 'left_4x4_l2' ).C;
	for ( j = 0; j < 4; j++ ) {
		for ( i = 0; i < 4; i++ ) {
			assert.ok( Math.abs( C[ ( j * LDC ) + i ] - expected[ ( j * 4 ) + i ] ) <= TOL, 'padded LDC matches expected' );
		}
	}
});
