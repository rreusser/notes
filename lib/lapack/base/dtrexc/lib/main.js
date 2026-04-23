'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrexc = require( './dtrexc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrexc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrexc;
