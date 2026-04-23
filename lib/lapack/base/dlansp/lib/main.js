
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlansp = require( './dlansp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlansp, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlansp;
