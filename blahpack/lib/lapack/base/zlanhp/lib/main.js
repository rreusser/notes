
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlanhp = require( './zlanhp.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlanhp, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlanhp;
