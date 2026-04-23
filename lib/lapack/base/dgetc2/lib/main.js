'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgetc2 = require( './dgetc2.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgetc2, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgetc2;
