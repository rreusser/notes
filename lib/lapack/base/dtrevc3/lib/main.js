'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrevc3 = require( './dtrevc3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrevc3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrevc3;
