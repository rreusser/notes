'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgttrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgttrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function toF64( cArr ) {
	return Array.prototype.slice.call( reinterpret( cArr, 0 ) );
}


// TESTS //

test( 'zgttrf: basic 4x4, no pivoting (diag dominant)', function t() {
	var tc = findCase( 'basic_4' );
	var DL = new Complex128Array( new Float64Array( [ 1.0, 0.0, 1.0, 0.5, 0.5, 0.0 ] ) );
	var D = new Complex128Array( new Float64Array( [ 4.0, 1.0, 5.0, -1.0, 6.0, 0.0, 3.0, 2.0 ] ) );
	var DU = new Complex128Array( new Float64Array( [ 2.0, 0.0, 1.0, -1.0, 0.5, 0.5 ] ) );
	var DU2 = new Complex128Array( 2 );
	var IPIV = new Int32Array( 4 );
	var info = zgttrf( 4, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( DL ), tc.DL, 1e-14, 'DL' );
	assertArrayClose( toF64( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( toF64( DU ), tc.DU, 1e-14, 'DU' );
	assertArrayClose( toF64( DU2 ), tc.DU2, 1e-14, 'DU2' );
	// Fortran IPIV is 1-based, JS is 0-based
	var i;
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgttrf: 4x4 with pivoting (small diag, large subdiag)', function t() {
	var tc = findCase( 'pivot_4' );
	var DL = new Complex128Array( new Float64Array( [ 10.0, 5.0, 8.0, 0.0, 7.0, -3.0 ] ) );
	var D = new Complex128Array( new Float64Array( [ 0.1, 0.0, 0.2, 0.1, 0.3, 0.0, 0.4, -0.1 ] ) );
	var DU = new Complex128Array( new Float64Array( [ 1.0, 1.0, 2.0, 0.0, 3.0, -1.0 ] ) );
	var DU2 = new Complex128Array( 2 );
	var IPIV = new Int32Array( 4 );
	var info = zgttrf( 4, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( DL ), tc.DL, 1e-14, 'DL' );
	assertArrayClose( toF64( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( toF64( DU ), tc.DU, 1e-14, 'DU' );
	assertArrayClose( toF64( DU2 ), tc.DU2, 1e-14, 'DU2' );
	var i;
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgttrf: N=1', function t() {
	var tc = findCase( 'n1' );
	var DL = new Complex128Array( 1 );
	var D = new Complex128Array( new Float64Array( [ 3.0, 2.0 ] ) );
	var DU = new Complex128Array( 1 );
	var DU2 = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var info = zgttrf( 1, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( D ), tc.D, 1e-14, 'D' );
	assert.strictEqual( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
});

test( 'zgttrf: N=2, no pivot', function t() {
	var tc = findCase( 'n2_nopivot' );
	var DL = new Complex128Array( new Float64Array( [ 3.0, 1.0 ] ) );
	var D = new Complex128Array( new Float64Array( [ 5.0, -1.0, 4.0, 2.0 ] ) );
	var DU = new Complex128Array( new Float64Array( [ 2.0, 0.0 ] ) );
	var DU2 = new Complex128Array( 1 );
	var IPIV = new Int32Array( 2 );
	var info = zgttrf( 2, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( DL ), tc.DL, 1e-14, 'DL' );
	assertArrayClose( toF64( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( toF64( DU ), tc.DU, 1e-14, 'DU' );
	var i;
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgttrf: N=2, with pivot', function t() {
	var tc = findCase( 'n2_pivot' );
	var DL = new Complex128Array( new Float64Array( [ 10.0, 5.0 ] ) );
	var D = new Complex128Array( new Float64Array( [ 0.1, 0.0, 3.0, 0.0 ] ) );
	var DU = new Complex128Array( new Float64Array( [ 1.0, 0.0 ] ) );
	var DU2 = new Complex128Array( 1 );
	var IPIV = new Int32Array( 2 );
	var info = zgttrf( 2, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( toF64( DL ), tc.DL, 1e-14, 'DL' );
	assertArrayClose( toF64( D ), tc.D, 1e-14, 'D' );
	assertArrayClose( toF64( DU ), tc.DU, 1e-14, 'DU' );
	var i;
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
	}
});

test( 'zgttrf: N=0', function t() {
	var DL = new Complex128Array( 1 );
	var D = new Complex128Array( 1 );
	var DU = new Complex128Array( 1 );
	var DU2 = new Complex128Array( 1 );
	var IPIV = new Int32Array( 1 );
	var info = zgttrf( 0, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});

test( 'zgttrf: singular matrix returns info > 0', function t() {
	var DL = new Complex128Array( new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] ) );
	var D = new Complex128Array( new Float64Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] ) );
	var DU = new Complex128Array( new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] ) );
	var DU2 = new Complex128Array( 1 );
	var IPIV = new Int32Array( 3 );
	var info = zgttrf( 3, DL, 1, 0, D, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	assert.ok( info > 0, 'info > 0 for singular matrix' );
});
