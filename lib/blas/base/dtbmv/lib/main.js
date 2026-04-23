
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtbmv = require( './dtbmv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtbmv, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtbmv;
