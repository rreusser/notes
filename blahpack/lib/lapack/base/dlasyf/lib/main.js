

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );

var dlasyf = require( './dlasyf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlasyf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlasyf;
