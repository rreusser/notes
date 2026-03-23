

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var zungbr = require( './zungbr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zungbr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zungbr;
