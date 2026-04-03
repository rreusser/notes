
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zspsv = require( './zspsv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zspsv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zspsv;
