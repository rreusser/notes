
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarzb = require( './dlarzb.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarzb, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarzb;
