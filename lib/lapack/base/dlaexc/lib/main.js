'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaexc = require( './dlaexc.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaexc, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaexc;
