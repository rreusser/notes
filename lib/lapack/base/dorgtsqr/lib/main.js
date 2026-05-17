
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dorgtsqr = require( './dorgtsqr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dorgtsqr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dorgtsqr;
