
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasyfAa = require( './dlasyf_aa.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasyfAa, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasyfAa;
