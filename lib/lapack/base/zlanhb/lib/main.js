'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlanhb = require( './zlanhb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlanhb, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlanhb;
