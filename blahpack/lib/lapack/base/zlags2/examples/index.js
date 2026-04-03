
'use strict';

var Complex128 = require( '@stdlib/complex/float64/ctor' ); // eslint-disable-line stdlib/require-globals
var zlags2 = require( './../lib' );

var out = zlags2( true, 4.0, new Complex128( 2.0, 1.0 ), 3.0, 1.0, new Complex128( 0.5, 0.25 ), 2.0 ); // eslint-disable-line max-len
console.log( out ); // eslint-disable-line no-console
