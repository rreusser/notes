'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsycon = require( './zsycon.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsycon, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsycon;
