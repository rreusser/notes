'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var zdscal = require( './../lib/base.js' );

// Scale a complex vector by a real constant:
var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );

zdscal( 2, 2.0, zx, 1, 0 );
console.log( zx ); // eslint-disable-line no-console
