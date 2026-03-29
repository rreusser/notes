

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var ztfttp = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztfttp.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function runCase( name, transr, uplo ) {
	var expected;
	var result;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;

	tc = findCase( name );
	N = tc.n;

	// Create Complex128Arrays from interleaved re/im fixture data:
	ARF = new Complex128Array( new Float64Array( tc.ARF ) );
	AP = new Complex128Array( ( N * ( N + 1 ) / 2 ) | 0 );
	expected = new Float64Array( tc.AP );

	info = ztfttp( transr, uplo, N, ARF, 1, 0, AP, 1, 0 );
	assert.equal( info, 0, name + ': info' );

	result = reinterpret( AP, 0 );
	assert.deepStrictEqual( result, expected, name + ': AP' );
}


// TESTS //

test( 'ztfttp is a function', function t() {
	assert.equal( typeof ztfttp, 'function' );
});

test( 'ztfttp: N=0 quick return', function t() {
	var info;
	var AP;

	AP = new Complex128Array( 3 );
	info = ztfttp( 'no-transpose', 'lower', 0, new Complex128Array( 0 ), 1, 0, AP, 1, 0 );
	assert.equal( info, 0, 'info should be 0' );
});

test( 'ztfttp: N=1, normal', function t() {
	var expected;
	var result;
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_N' );
	ARF = new Complex128Array( new Float64Array( [ 42.0, 7.0 ] ) );
	AP = new Complex128Array( 1 );
	info = ztfttp( 'no-transpose', 'lower', 1, ARF, 1, 0, AP, 1, 0 );

	assert.equal( info, 0, 'info' );
	result = reinterpret( AP, 0 );
	expected = new Float64Array( tc.AP );
	assert.deepStrictEqual( result, expected, 'AP' );
});

test( 'ztfttp: N=1, conjugate-transpose', function t() {
	var expected;
	var result;
	var info;
	var ARF;
	var tc;
	var AP;

	tc = findCase( 'n1_C' );
	ARF = new Complex128Array( new Float64Array( [ 99.0, -3.0 ] ) );
	AP = new Complex128Array( 1 );
	info = ztfttp( 'conjugate-transpose', 'upper', 1, ARF, 1, 0, AP, 1, 0 );

	assert.equal( info, 0, 'info' );
	result = reinterpret( AP, 0 );
	expected = new Float64Array( tc.AP );
	assert.deepStrictEqual( result, expected, 'AP' );
});

// N=5 (odd) — all 4 combinations:
test( 'ztfttp: N=5, no-transpose, lower', function t() {
	runCase( 'n5_N_L', 'no-transpose', 'lower' );
});

test( 'ztfttp: N=5, no-transpose, upper', function t() {
	runCase( 'n5_N_U', 'no-transpose', 'upper' );
});

test( 'ztfttp: N=5, conjugate-transpose, lower', function t() {
	runCase( 'n5_C_L', 'conjugate-transpose', 'lower' );
});

test( 'ztfttp: N=5, conjugate-transpose, upper', function t() {
	runCase( 'n5_C_U', 'conjugate-transpose', 'upper' );
});

// N=6 (even) — all 4 combinations:
test( 'ztfttp: N=6, no-transpose, lower', function t() {
	runCase( 'n6_N_L', 'no-transpose', 'lower' );
});

test( 'ztfttp: N=6, no-transpose, upper', function t() {
	runCase( 'n6_N_U', 'no-transpose', 'upper' );
});

test( 'ztfttp: N=6, conjugate-transpose, lower', function t() {
	runCase( 'n6_C_L', 'conjugate-transpose', 'lower' );
});

test( 'ztfttp: N=6, conjugate-transpose, upper', function t() {
	runCase( 'n6_C_U', 'conjugate-transpose', 'upper' );
});

// N=7 (odd, larger) — all 4 combinations:
test( 'ztfttp: N=7, no-transpose, lower', function t() {
	runCase( 'n7_N_L', 'no-transpose', 'lower' );
});

test( 'ztfttp: N=7, no-transpose, upper', function t() {
	runCase( 'n7_N_U', 'no-transpose', 'upper' );
});

test( 'ztfttp: N=7, conjugate-transpose, lower', function t() {
	runCase( 'n7_C_L', 'conjugate-transpose', 'lower' );
});

test( 'ztfttp: N=7, conjugate-transpose, upper', function t() {
	runCase( 'n7_C_U', 'conjugate-transpose', 'upper' );
});

// N=8 (even, larger) — all 4 combinations:
test( 'ztfttp: N=8, no-transpose, lower', function t() {
	runCase( 'n8_N_L', 'no-transpose', 'lower' );
});

test( 'ztfttp: N=8, no-transpose, upper', function t() {
	runCase( 'n8_N_U', 'no-transpose', 'upper' );
});

test( 'ztfttp: N=8, conjugate-transpose, lower', function t() {
	runCase( 'n8_C_L', 'conjugate-transpose', 'lower' );
});

test( 'ztfttp: N=8, conjugate-transpose, upper', function t() {
	runCase( 'n8_C_U', 'conjugate-transpose', 'upper' );
});

// Test with non-unit strides:
test( 'ztfttp: N=5, no-transpose, lower, strideARF=2', function t() {
	var expected;
	var result;
	var ARFdata;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;
	var i;

	tc = findCase( 'n5_N_L' );
	N = tc.n;
	expected = new Float64Array( tc.AP );

	// Create ARF with stride 2 (interleave with dummy complex elements):
	ARFdata = new Float64Array( tc.ARF.length * 2 );
	for ( i = 0; i < tc.ARF.length / 2; i += 1 ) {
		ARFdata[ i * 4 ] = tc.ARF[ i * 2 ];
		ARFdata[ ( i * 4 ) + 1 ] = tc.ARF[ ( i * 2 ) + 1 ];
	}
	ARF = new Complex128Array( ARFdata );
	AP = new Complex128Array( ( N * ( N + 1 ) / 2 ) | 0 );

	info = ztfttp( 'no-transpose', 'lower', N, ARF, 2, 0, AP, 1, 0 );
	assert.equal( info, 0, 'info' );

	result = reinterpret( AP, 0 );
	assert.deepStrictEqual( result, expected, 'AP with strideARF=2' );
});

test( 'ztfttp: N=5, no-transpose, lower, strideAP=2', function t() {
	var result;
	var info;
	var ARF;
	var tc;
	var AP;
	var N;
	var i;

	tc = findCase( 'n5_N_L' );
	N = tc.n;
	ARF = new Complex128Array( new Float64Array( tc.ARF ) );
	AP = new Complex128Array( ( N * ( N + 1 ) / 2 ) * 2 );

	info = ztfttp( 'no-transpose', 'lower', N, ARF, 1, 0, AP, 2, 0 );
	assert.equal( info, 0, 'info' );

	// Check that every other complex element matches:
	result = reinterpret( AP, 0 );
	for ( i = 0; i < tc.AP.length / 2; i += 1 ) {
		assert.equal( result[ i * 4 ], tc.AP[ i * 2 ], 'AP re[' + i + ']' );
		assert.equal( result[ ( i * 4 ) + 1 ], tc.AP[ ( i * 2 ) + 1 ], 'AP im[' + i + ']' );
	}
});

test( 'ztfttp: N=6, conjugate-transpose, upper, with offset', function t() {
	var expected;
	var ARFdata;
	var result;
	var info;
	var ARF;
	var tc;
	var AP;
	var NT;
	var N;
	var i;

	tc = findCase( 'n6_C_U' );
	N = tc.n;
	NT = ( N * ( N + 1 ) / 2 ) | 0;
	expected = new Float64Array( tc.AP );

	// Prepend 3 dummy complex elements:
	ARFdata = new Float64Array( tc.ARF.length + 6 );
	for ( i = 0; i < tc.ARF.length; i += 1 ) {
		ARFdata[ i + 6 ] = tc.ARF[ i ];
	}
	ARF = new Complex128Array( ARFdata );
	AP = new Complex128Array( NT + 5 );

	info = ztfttp( 'conjugate-transpose', 'upper', N, ARF, 1, 3, AP, 1, 5 );
	assert.equal( info, 0, 'info' );

	// Check AP starting at offset 5 (complex elements = 10 doubles):
	result = reinterpret( AP, 0 );
	for ( i = 0; i < tc.AP.length; i += 1 ) {
		assert.equal( result[ i + 10 ], expected[ i ], 'AP[' + ( i + 10 ) + ']' );
	}
});
