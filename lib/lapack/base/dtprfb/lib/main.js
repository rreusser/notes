
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtprfb = require( './dtprfb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtprfb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtprfb;
