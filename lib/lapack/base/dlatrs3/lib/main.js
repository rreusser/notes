
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlatrs3 = require( './dlatrs3.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlatrs3, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlatrs3;
