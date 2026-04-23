
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dtbrfs = require( './dtbrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dtbrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = dtbrfs;
