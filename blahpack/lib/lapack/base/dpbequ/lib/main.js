'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dpbequ = require( './dpbequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dpbequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = dpbequ;
