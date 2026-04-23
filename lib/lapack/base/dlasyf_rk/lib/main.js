
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlasyfRk = require( './dlasyf_rk.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasyfRk, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasyfRk;
