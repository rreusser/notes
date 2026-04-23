
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var ztrrfs = require( './ztrrfs.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( ztrrfs, 'ndarray', ndarray );


// EXPORTS //

module.exports = ztrrfs;
