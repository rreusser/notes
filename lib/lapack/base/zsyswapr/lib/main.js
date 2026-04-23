
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zsyswapr = require( './zsyswapr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zsyswapr, 'ndarray', ndarray );


// EXPORTS //

module.exports = zsyswapr;
