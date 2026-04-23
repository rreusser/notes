
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarrr = require( './dlarrr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarrr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarrr;
