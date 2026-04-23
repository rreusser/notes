'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlaed5 = require( './dlaed5.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlaed5, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlaed5;
