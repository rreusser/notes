'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsysv = require( './zsysv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsysv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsysv;
