'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dppequ = require( './dppequ.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dppequ, 'ndarray', ndarray );


// EXPORTS //

module.exports = dppequ;
