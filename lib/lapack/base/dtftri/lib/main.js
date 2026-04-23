
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtftri = require( './dtftri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtftri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtftri;
