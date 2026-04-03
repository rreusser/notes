'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspcon = require( './dspcon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspcon, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspcon;
