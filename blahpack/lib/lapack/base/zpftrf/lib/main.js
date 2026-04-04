
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zpftrf = require( './zpftrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zpftrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zpftrf;
