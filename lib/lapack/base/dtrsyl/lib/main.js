'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtrsyl = require( './dtrsyl.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtrsyl, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtrsyl;
