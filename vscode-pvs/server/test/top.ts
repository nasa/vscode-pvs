import { PvsProcess } from "./server/pvsProcess";
import { PvsResponseType } from "./server/common/serverInterface";

describe("PvsServer", async () => {
	const pvsPath: string = "/Users/pmasci/Work/pvs-snapshots/pvs-7.0-851";
	it("should be able to create a pvs process", async () => {
		let proc = new PvsProcess({
			pvsPath,
			pvsContextFolder: "./"
		});
		const success = await proc.pvs();
		expect(success).toBeTruthy();
		const versionInfo: PvsResponseType = await proc.pvsVersionInformation();
		expect(versionInfo).not.toBeNull();
		expect(versionInfo.res).not.toBeNull();
		console.log(versionInfo.res);
	});
});
