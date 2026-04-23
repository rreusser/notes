
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlanhf = require( './zlanhf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlanhf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlanhf;
