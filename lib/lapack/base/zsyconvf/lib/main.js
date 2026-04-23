
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyconvf = require( './zsyconvf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyconvf, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyconvf;
