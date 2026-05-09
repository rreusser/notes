
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtzrzf = require( './dtzrzf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtzrzf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtzrzf;
