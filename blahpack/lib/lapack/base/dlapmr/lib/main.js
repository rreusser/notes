'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlapmr = require( './dlapmr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlapmr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlapmr;
