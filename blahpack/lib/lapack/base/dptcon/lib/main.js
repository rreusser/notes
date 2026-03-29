'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dptcon = require( './dptcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dptcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dptcon;
