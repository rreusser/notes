
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpftri = require( './zpftri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpftri, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpftri;
