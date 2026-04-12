/**
* @license Apache-2.0
*
* Copyright (c) 2025 Ricky Reusser.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*/

'use strict';

// MODULES //

var bench = require( '@stdlib/bench' );
var isnan = require( '@stdlib/math/base/assert/is-nan' );
var Float64Array = require( '@stdlib/array/float64' );
var pkg = require( './../package.json' ).name;
var dlartgp = require( './../lib/ndarray.js' );


// MAIN //

/**
* Benchmark ndarray export.
*
* @private
* @param {Benchmark} b - benchmark instance
*/
function benchmark( b ) {
	var out;
	var i;

	out = new Float64Array( 3 );
	b.tic();
	for ( i = 0; i < b.iterations; i++ ) {
		dlartgp( ( i % 7 ) + 1.0, ( i % 5 ) + 1.0, out );
		if ( isnan( out[ 2 ] ) ) {
			b.fail( 'should not return NaN' );
		}
	}
	b.toc();
	if ( isnan( out[ 2 ] ) ) {
		b.fail( 'should not return NaN' );
	}
	b.pass( 'benchmark finished' );
	b.end();
}

bench( pkg + ':ndarray', benchmark );
