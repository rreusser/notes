
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarzt = require( './dlarzt.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarzt, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarzt;
